package harness.http.server

import cats.data.NonEmptyList
import com.sun.net.httpserver.*
import harness.core.*
import harness.zio.*
import java.io.{ByteArrayInputStream, FileInputStream, InputStream, OutputStream}
import java.net.InetSocketAddress
import java.security.{KeyStore, SecureRandom}
import javax.net.ssl.*
import zio.*

object Server {

  /**
    * Note, this will never return
    */
  def start[ServerEnv, ReqEnv: EnvironmentTag](
      reqLayer: SHRLayer[ServerEnv & Scope, ReqEnv],
  )(
      route: Route[ServerEnv & ReqEnv],
  ): SHRIO[ServerEnv & ServerConfig, Nothing] =
    for {
      config <- ZIO.service[ServerConfig]
      port = config.port.getOrElse(if (config.ssl.nonEmpty) 443 else 8080)
      _ <- Logger.log.info(s"Starting server on port $port")
      inet <- ZIO.hAttempt(InetSocketAddress(port))
      server <- createHttpServer(config, inet)
      runtime <- ZIO.runtime[HarnessEnv & ServerEnv]
      handler = Handler(runtime, reqLayer, route)
      _ <- ZIO.hAttempt(server.createContext("/", handler))
      _ <- ZIO.hAttempt(server.setExecutor(null))
      _ <- ZIO.hAttempt(server.start())
      nothing <- ZIO.never // TODO (KR) : do this or "press any key to continue"?
    } yield nothing

  private def createHttpServer(config: ServerConfig, inet: InetSocketAddress): SHTask[HttpServer] =
    config.ssl match {
      case Some(sslConfig) =>
        for {
          server <- ZIO.hAttempt(HttpsServer.create(inet, 0))
          _ <- Logger.log.detailed("Configuring SSL for http server")
          _ <- configureSSL(server, sslConfig)
        } yield server
      case None =>
        Logger.log.warning("No SSL configured for http server") *>
          ZIO.hAttempt(HttpServer.create(inet, 0))
    }

  private def configureSSL(server: HttpsServer, sslConfig: ServerConfig.SslConfig): HRIO[FileSystem & Logger, Unit] = {
    def wrapUnsafe[A](hint: String)(thunk: => A): HTask[A] =
      ZIO.hAttempt { thunk }.mapError(HError.InternalDefect(s"Error during SSL configuration: $hint", _))

    def getInputStream(refType: ServerConfig.SslConfig.RefType, ref: String): HRIO[FileSystem & Scope, InputStream] =
      refType match {
        case ServerConfig.SslConfig.RefType.Str  => ZIO.succeed(new ByteArrayInputStream(ref.getBytes))
        case ServerConfig.SslConfig.RefType.Jar  => JarUtils.getInputStream(ref)
        case ServerConfig.SslConfig.RefType.File => Path(ref).flatMap(_.inputStream)
      }

    ZIO.scoped {
      for {
        // Load keystore
        keystore <- wrapUnsafe("KeyStore.getInstance") { KeyStore.getInstance("JKS") }
        keystoreInputStream <- getInputStream(sslConfig.keystoreRefType, sslConfig.keystoreRef)
        _ <- wrapUnsafe("keystore.load") { keystore.load(keystoreInputStream, sslConfig.keystorePassword.toCharArray) }

        // Load truststore
        trustStore <- wrapUnsafe("KeyStore.getInstance") { KeyStore.getInstance("JKS") }
        trustStoreStream <- getInputStream(sslConfig.truststoreRefType, sslConfig.truststoreRef)
        _ <- wrapUnsafe("trustStore.load") { trustStore.load(trustStoreStream, sslConfig.truststorePassword.toCharArray) }

        // Initialize KeyManagerFactory and TrustManagerFactory
        keyManagerFactory <- wrapUnsafe("KeyManagerFactory.getInstance") { KeyManagerFactory.getInstance("SunX509") }
        _ <- wrapUnsafe("keyManagerFactory.init") { keyManagerFactory.init(keystore, sslConfig.keystorePassword.toCharArray) }
        trustManagerFactory <- wrapUnsafe("TrustManagerFactory.getInstance") { TrustManagerFactory.getInstance("SunX509") }
        _ <- wrapUnsafe("trustManagerFactory.init") { trustManagerFactory.init(keystore) }

        // Initialize SSLContext
        sslContext <- wrapUnsafe("SSLContext.getInstance") { SSLContext.getInstance("TLS") }
        _ <- wrapUnsafe("sslContext.init") { sslContext.init(keyManagerFactory.getKeyManagers, trustManagerFactory.getTrustManagers, new SecureRandom()) }

        _ <- wrapUnsafe("server.setHttpsConfigurator") {
          server.setHttpsConfigurator(
            new HttpsConfigurator(sslContext) {
              override def configure(params: HttpsParameters): Unit = {
                val engine = sslContext.createSSLEngine
                params.setNeedClientAuth(false)
                params.setCipherSuites(engine.getEnabledCipherSuites)
                params.setProtocols(engine.getEnabledProtocols)

                val defaultSSLParameters = sslContext.getDefaultSSLParameters
                params.setSSLParameters(defaultSSLParameters)
              }
            },
          )
        }
      } yield ()
    }
  }

}
