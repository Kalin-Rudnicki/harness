package harness.http.server

import com.sun.net.httpserver.*
import harness.zio.*
import java.io.{ByteArrayInputStream, InputStream}
import java.net.InetSocketAddress
import java.security.{KeyFactory, KeyStore, SecureRandom}
import java.security.cert.CertificateFactory
import java.security.spec.PKCS8EncodedKeySpec
import java.util.Base64
import javax.net.ssl.*
import zio.*

object Server {

  /**
    * Note, this will never return
    */
  def start[ServerEnv, ReqEnv: EnvironmentTag](
      reqLayer: RLayer[HarnessEnv & ServerEnv & Scope, ReqEnv],
  )(
      route: Route[ServerEnv & ReqEnv],
  ): RIO[HarnessEnv & ServerEnv & ServerConfig, Nothing] =
    for {
      config <- ZIO.service[ServerConfig]
      port = config.port.getOrElse(if (config.ssl.nonEmpty) 443 else 8080)
      _ <- Logger.log.info(s"Starting server on port $port")
      inet <- ZIO.attempt(InetSocketAddress(port))
      server <- createHttpServer(config, inet)
      runtime <- ZIO.runtime[HarnessEnv & ServerEnv]
      handler = Handler(runtime, reqLayer, route, config.debugErrorHeader)
      _ <- ZIO.attempt(server.createContext("/", handler))
      _ <- ZIO.attempt(server.setExecutor(null))
      _ <- ZIO.attempt(server.start())
      nothing <- ZIO.never // TODO (KR) : do this or "press any key to continue"?
    } yield nothing

  private def createHttpServer(config: ServerConfig, inet: InetSocketAddress): RIO[HarnessEnv, HttpServer] =
    config.ssl match {
      case Some(sslConfig) =>
        for {
          server <- ZIO.attempt(HttpsServer.create(inet, 0))
          _ <- Logger.log.detailed("Configuring SSL for http server")
          _ <- configureSSL(server, sslConfig)
        } yield server
      case None =>
        Logger.log.warning("No SSL configured for http server") *>
          ZIO.attempt(HttpServer.create(inet, 0))
    }

  // TODO (KR) : fix bug where only first request fails to load
  //           : NOTE - it is not "failing to load", it is just very slow
  private def configureSSL(server: HttpsServer, sslConfig: ServerConfig.SslConfig): RIO[FileSystem & Logger, Unit] = {
    def wrapUnsafe[A](hint: String)(thunk: => A): Task[A] =
      ZIO.attempt { thunk }.mapError(new RuntimeException(s"Error during SSL configuration: $hint", _))

    def getInputStream(refType: ServerConfig.SslConfig.RefType, ref: String): RIO[FileSystem & Scope, InputStream] =
      refType match {
        case ServerConfig.SslConfig.RefType.Str  => ZIO.succeed(new ByteArrayInputStream(ref.getBytes))
        case ServerConfig.SslConfig.RefType.Jar  => JarUtils.getInputStream(ref)
        case ServerConfig.SslConfig.RefType.File => Path(ref).flatMap(_.inputStream)
      }

    val keyStorePassword = sslConfig.certificatePassword.map(_.toCharArray).orNull

    ZIO.scoped {
      for {
        // Load certificate chain
        certificateStream <- getInputStream(sslConfig.certificateRefType, sslConfig.certificateRef)
        certificate <- wrapUnsafe("CertificateFactory.getInstance") { CertificateFactory.getInstance("X.509").generateCertificate(certificateStream) }

        // Load certificate into keystore
        keyStore <- wrapUnsafe("KeyStore.getInstance") { KeyStore.getInstance("PKCS12") }
        _ <- wrapUnsafe("keystore.load") { keyStore.load(null, keyStorePassword) }
        _ <- wrapUnsafe("keyStore.setCertificateEntry") { keyStore.setCertificateEntry("cert", certificate) }

        // Load private key into keystore
        privateKeyStream <- getInputStream(sslConfig.privateKeyRefType, sslConfig.privateKeyRef)
        privateKeyBytes <- wrapUnsafe("privateKeyStream.readAllBytes") { privateKeyStream.readAllBytes() }
        privateKeyPEM = new String(privateKeyBytes)
        privateKeyPEMContent =
          privateKeyPEM
            .replace("-----BEGIN PRIVATE KEY-----", "")
            .replace("-----END PRIVATE KEY-----", "")
            .replaceAll("\\s", "")
        decodedKey <- wrapUnsafe("Base64.getDecoder.decode") { Base64.getDecoder.decode(privateKeyPEMContent) }
        keyFactory <- wrapUnsafe("KeyFactory.getInstance") { KeyFactory.getInstance("RSA") }
        privateKeySpec <- wrapUnsafe("new PKCS8EncodedKeySpec") { new PKCS8EncodedKeySpec(decodedKey) }
        privateKey <- wrapUnsafe("keyFactory.generatePrivate") { keyFactory.generatePrivate(privateKeySpec) }
        _ <- wrapUnsafe("keyStore.setKeyEntry") { keyStore.setKeyEntry("key", privateKey, keyStorePassword, Array(certificate)) }

        // Initialize KeyManagerFactory and TrustManagerFactory
        keyManagerFactory <- wrapUnsafe("KeyManagerFactory.getInstance") { KeyManagerFactory.getInstance("SunX509") }
        _ <- wrapUnsafe("keyManagerFactory.init") { keyManagerFactory.init(keyStore, keyStorePassword) }

        // Initialize SSLContext
        sslContext <- wrapUnsafe("SSLContext.getInstance") { SSLContext.getInstance("TLS") }
        _ <- wrapUnsafe("sslContext.init") { sslContext.init(keyManagerFactory.getKeyManagers, null, new SecureRandom()) }
        engine <- wrapUnsafe("sslContext.createSSLEngine") { sslContext.createSSLEngine }
        defaultSSLParameters <- wrapUnsafe("sslContext.getDefaultSSLParameters") { sslContext.getDefaultSSLParameters }

        _ <- wrapUnsafe("server.setHttpsConfigurator") {
          server.setHttpsConfigurator(
            new HttpsConfigurator(sslContext) {
              override def configure(params: HttpsParameters): Unit = {
                params.setNeedClientAuth(false)
                params.setCipherSuites(engine.getEnabledCipherSuites)
                params.setProtocols(engine.getEnabledProtocols)
                params.setSSLParameters(defaultSSLParameters)
              }
            },
          )
        }
      } yield ()
    }
  }

}
