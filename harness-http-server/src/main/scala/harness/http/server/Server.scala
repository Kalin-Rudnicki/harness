package harness.http.server

import cats.data.NonEmptyList
import com.sun.net.httpserver.*
import harness.core.*
import harness.zio.*
import java.io.{FileInputStream, InputStream, OutputStream}
import java.net.InetSocketAddress
import java.security.KeyStore
import javax.net.ssl.*
import zio.*

object Server {

  def start[ServerEnv, ReqEnv: EnvironmentTag](
      reqLayer: SHRLayer[ServerEnv & Scope, ReqEnv],
  )(
      route: Route[ServerEnv & ReqEnv],
  ): SHRIO[ServerEnv & ServerConfig, Unit] = {
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
      _ <- ZIO.never // TODO (KR) : do this or "press any key to continue"?
    } yield ()
  }

  private def createHttpServer(config: ServerConfig, inet: InetSocketAddress): SHTask[HttpServer] =
    config.ssl match {
      case Some(sslConfig) =>
        for {
          server <- ZIO.hAttempt(HttpsServer.create(inet, 0))
          _ <- configureSSL(server, sslConfig)
        } yield server
      case None =>
        ZIO.hAttempt(HttpServer.create(inet, 0))
    }

  private def configureSSL(server: HttpsServer, sslConfig: ServerConfig.SslConfig): HTask[Unit] =
    ZIO
      .hAttempt {
        val sslContext = SSLContext.getInstance("TLS")
        val password = sslConfig.keyPassword.toCharArray
        val ks = KeyStore.getInstance("JKS")
        val fis = new FileInputStream(sslConfig.keyPath)
        ks.load(fis, password)

        val kmf = KeyManagerFactory.getInstance("SunX509")
        kmf.init(ks, password)

        val tmf = TrustManagerFactory.getInstance("SunX509")
        tmf.init(ks)

        sslContext.init(kmf.getKeyManagers, tmf.getTrustManagers, null)
        server.setHttpsConfigurator(
          new HttpsConfigurator(sslContext) {
            override def configure(params: HttpsParameters): Unit = {
              val c = SSLContext.getDefault
              val engine = c.createSSLEngine
              params.setNeedClientAuth(false)
              params.setCipherSuites(engine.getEnabledCipherSuites)
              params.setProtocols(engine.getEnabledProtocols)

              val defaultSSLParameters = c.getDefaultSSLParameters
              params.setSSLParameters(defaultSSLParameters)
            }
          },
        )
      }
      .mapError(HError.InternalDefect("Unable to configure SSL", _))

}
