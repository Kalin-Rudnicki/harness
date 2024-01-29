package harness.http.server

import harness.cli.*
import harness.http.server.ServerConfig.SslConfig
import harness.zio.*
import java.util.UUID
import zio.json.*

object GenSslKeys extends ExecutableApp {

  final case class Cfg(
      alias: String,
      jarResDir: String,
      keyStorePass: Option[String],
      trustStorePass: Option[String],
      resourcePrefix: String,
  )
  object Cfg {

    val parser: Parser[Cfg] = {
      Parser.value[String](LongName.unsafe("alias")) &&
      Parser.value[String](LongName.unsafe("jar-res-dir")) &&
      Parser.value[String](LongName.unsafe("key-store-pass")).optional &&
      Parser.value[String](LongName.unsafe("trust-store-pass")).optional &&
      Parser.value[String](LongName.unsafe("resource-prefix")).default("ssl")
    }.map { Cfg.apply }

  }

  override val executable: Executable =
    Executable
      .withParser(Cfg.parser)
      .withEffect { cfg =>
        for {
          _ <- Logger.log.info("Running key generation")

          baseKeystore = "keystore.jks"
          baseTruststore = "truststore.jks"
          resourcePrefix = if (cfg.resourcePrefix.isEmpty) "" else s"${cfg.resourcePrefix}/"
          keystoreRef = s"$resourcePrefix$baseKeystore"
          truststoreRef = s"$resourcePrefix$baseTruststore"
          keystoreInRes = s"${cfg.jarResDir}/$keystoreRef"
          truststoreInRes = s"${cfg.jarResDir}/$truststoreRef"
          certFile = "server.crt"
          validity = "365"

          keystorePass = cfg.keyStorePass.getOrElse(UUID.randomUUID.toString)
          truststorePass = cfg.trustStorePass.getOrElse(UUID.randomUUID.toString)

          makeParentDirIfDNE =
            (path: String) =>
              for {
                file <- Path(path)
                parentFile <- file.absolutePath.parent
                _ <- parentFile.mkdirs.unlessZIO(parentFile.exists)
              } yield ()

          _ <- Logger.log.debug("step 0")
          _ <- makeParentDirIfDNE(keystoreInRes)
          _ <- makeParentDirIfDNE(truststoreInRes)

          _ <- Logger.log.debug("step 1")
          _ <- Sys.execute0(
            "keytool",
            "-genkeypair",
            "-keyalg",
            "RSA",
            "-keysize",
            "2048",
            "-alias",
            cfg.alias,
            "-keystore",
            baseKeystore,
            "-storepass",
            keystorePass,
            "-validity",
            validity,
          )

          _ <- Logger.log.debug("step 2")
          _ <- Sys.execute0(
            "keytool",
            "-export",
            "-alias",
            cfg.alias,
            "-keystore",
            baseKeystore,
            "-storepass",
            keystorePass,
            "-file",
            certFile,
          )

          _ <- Logger.log.debug("step 3")
          _ <- Sys.execute0(
            "keytool",
            "-import",
            "-alias",
            cfg.alias,
            "-file",
            certFile,
            "-keystore",
            baseTruststore,
            "-storepass",
            truststorePass,
          )

          _ <- Logger.log.debug("step 4")
          _ <- Sys.execute0("rm", certFile)
          _ <- Sys.execute0("mv", baseKeystore, keystoreInRes)
          _ <- Sys.execute0("mv", baseTruststore, truststoreInRes)

          cfg = SslConfig(
            keystoreRef = keystoreRef,
            keystorePassword = keystorePass,
            keystoreRefType = SslConfig.RefType.Jar,
            truststoreRef = truststoreRef,
            truststorePassword = truststorePass,
            truststoreRefType = SslConfig.RefType.Jar,
          )
          _ <- Logger.log.important(s"Config:\n${cfg.toJson}")
        } yield ()
      }

}
