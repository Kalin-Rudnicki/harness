package harness.http.server

import com.sun.net.httpserver.*
import harness.core.*
import harness.endpoint.typeclass.Flatten
import harness.endpoint.types.EndpointType
import harness.schema.*
import harness.zio.*
import harness.zio.config.*
import java.net.InetSocketAddress
import java.security.{KeyFactory, KeyStore, SecureRandom}
import java.security.cert.CertificateFactory
import java.security.spec.PKCS8EncodedKeySpec
import java.util.Base64
import javax.net.ssl.*
import zio.*
import zio.json.*

object Server {

  final case class Config(
      port: Option[Int],
      resDir: String,
      useJarResource: Boolean,
      ssl: Option[Config.SslConfig],
      debugErrorHeader: Boolean,
      // TODO (KR) : Options relating to logging requests
  ) derives JsonSchema
  object Config {

    final case class SslConfig(
        // certificate
        certificate: ConfigSource,
        certificatePassword: Option[String],
        // private key
        privateKey: ConfigSource,
    ) derives JsonCodec
    object SslConfig {

      private implicit val configSourceJsonSchema: JsonSchema[ConfigSource] = JsonSchema.derived

      implicit val jsonSchema: JsonSchema[SslConfig] = JsonSchema.derived

    }

  }

  def start[ServerEnv, ReqEnv: EnvironmentTag, T[_[_ <: EndpointType.Any]], ImplR >: ServerEnv & ReqEnv](
      reqLayer: RLayer[ServerEnv & Scope, ReqEnv],
      endpoints: T[Endpoint.Projection[ImplR]],
      // TODO (KR) : docs
  )(implicit
      flatten: Flatten[T],
  ): RIO[ServerEnv & Config, Nothing] =
    for {
      config <- ZIO.service[Config]
      port = config.port.getOrElse(if (config.ssl.nonEmpty) 443 else 8080)
      _ <- Logger.log.info(s"Starting server on port $port")
      inet <- ZIO.attempt(InetSocketAddress(port))
      server <- createHttpServer(config, inet)
      runtime <- ZIO.runtime[ServerEnv]

      endpointList = flatten.flatten(endpoints)
      handler = Handler(runtime, reqLayer, endpointList, config.debugErrorHeader)
      _ <- ZIO.attempt(server.createContext("/", handler))
      _ <- ZIO.attempt(server.setExecutor(null))
      _ <- ZIO.attempt(server.start())

      nothing <- ZIO.never // TODO (KR) : do this or "press any key to continue"?
    } yield nothing

  private def createHttpServer(config: Config, inet: InetSocketAddress): Task[HttpServer] =
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
  private def configureSSL(server: HttpsServer, sslConfig: Config.SslConfig): Task[Unit] = {
    def wrapUnsafe[A](hint: String)(thunk: => A): Task[A] =
      ZIO.attempt { thunk }.mapError(new RuntimeException(s"Error during SSL configuration: $hint", _))

    val keyStorePassword = sslConfig.certificatePassword.map(_.toCharArray).orNull

    ZIO.scoped {
      for {
        // Load certificate chain
        certificateStream <- sslConfig.certificate.readInputStream
        certificate <- wrapUnsafe("CertificateFactory.getInstance") { CertificateFactory.getInstance("X.509").generateCertificate(certificateStream) }

        // Load certificate into keystore
        keyStore <- wrapUnsafe("KeyStore.getInstance") { KeyStore.getInstance("PKCS12") }
        _ <- wrapUnsafe("keystore.load") { keyStore.load(null, keyStorePassword) }
        _ <- wrapUnsafe("keyStore.setCertificateEntry") { keyStore.setCertificateEntry("cert", certificate) }

        // Load private key into keystore
        privateKeyStream <- sslConfig.privateKey.readInputStream
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
