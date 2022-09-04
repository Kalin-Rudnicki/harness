package harness.web.server

import com.sun.net.httpserver.*
import harness.zio.*
import java.io.{FileInputStream, InputStream, OutputStream}
import java.net.InetSocketAddress
import java.security.KeyStore
import javax.net.ssl.*
import zio.*

object Server {

  def start[R](
      config: ServerConfig,
  )(
      route: Route[R],
  ): SHRION[ServerEnv & R, Unit] = {
    val port: Int = config.port.getOrElse(if (config.sslConfig.nonEmpty) 443 else 8080)
    for {
      _ <- Logger.log.info(s"Starting server on port $port")
      inet <- ZIO.hAttemptNel("Error creating inet address")(InetSocketAddress(port))
      server <- createHttpServer(config, inet)
      runtime <- ZIO.runtime[HarnessEnv & ServerEnv & R]
      handler = Handler(runtime, route)
      _ <- ZIO.hAttemptNel("Error setting server context")(server.createContext("/", handler))
      _ <- ZIO.hAttemptNel("Error setting executor")(server.setExecutor(null))
      _ <- ZIO.hAttemptNel("Error starting server")(server.start())
      _ <- ZIO.never // TODO (KR) : do this or "press any key to continue"?
    } yield ()
  }

  private def createHttpServer(config: ServerConfig, inet: InetSocketAddress): SHTaskN[HttpServer] =
    config.sslConfig match {
      case Some(sslConfig) =>
        for {
          server <- ZIO.hAttemptNel("Error creating https-server")(HttpsServer.create(inet, 0))
          _ <- ZIO.hAttemptNel("Error applying ssl config")(configureSSL(server, sslConfig))
        } yield server
      case None =>
        ZIO.hAttemptNel("Error creating http-server")(HttpServer.create(inet, 0))
    }

  // TODO (KR) : Possibly zio-ify this
  private def configureSSL(server: HttpsServer, sslConfig: ServerConfig.SslConfig): Unit = {
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

}
