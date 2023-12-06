package harness.email

import harness.core.HError
import harness.zio.*
import java.util.Properties
import javax.mail.*
import javax.mail.internet.{InternetAddress, MimeMessage}
import zio.*
import zio.json.*

trait EmailClient {
  def sendEmail(email: SendEmail): HRIO[Logger & Telemetry, Unit]
}
object EmailClient {

  def sendEmail(email: SendEmail): HRIO[EmailClient & Logger & Telemetry, Unit] =
    ZIO.serviceWithZIO[EmailClient](_.sendEmail(email))

  // =====| Live |=====

  val liveLayer: HRLayer[EmailConfig & Logger, EmailClient] =
    ZLayer.fromZIO {
      ZIO.serviceWithZIO[EmailConfig] { config =>
        Logger.log.warning("Email config does not have auth enabled").when(config.passwordMap.isEmpty) *>
          ZIO.hAttempt { Live(config) }.mapError(HError.InternalDefect("Unable to create EmailClient.Live", _))
      }
    }

  final case class Live(config: EmailConfig) extends EmailClient {

    private val props: Properties = {
      val props = new Properties()

      props.put("mail.smtp.host", config.host)
      props.put("mail.smtp.port", config.port.toString)
      props.put("mail.smtp.auth", config.passwordMap.nonEmpty.toString)
      config.authType match {
        case EmailConfig.AuthType.NoAuth =>
        case EmailConfig.AuthType.TLS =>
          props.put("mail.smtp.starttls.enable", "true")
        case EmailConfig.AuthType.SSL =>
          props.put("mail.smtp.socketFactory.port", config.port.toString)
          props.put("mail.smtp.socketFactory.class", "javax.net.ssl.SSLSocketFactory")
      }

      props
    }

    override def sendEmail(email: SendEmail): HRIO[Logger & Telemetry, Unit] =
      for {
        _ <- Logger.log.debug(s"EmailClient.Live.sendEmail: ${email.toJson}")
        authenticator <- makeAuthenticator(email)
        session <- makeSession(authenticator)
        message <- makeMessage(session)
        _ <- configureMessage(message, email)
        _ <- sendMessage(message)
      } yield ()

    // =====| helpers |=====

    private def makeAuthenticator(email: SendEmail): HTask[Option[Authenticator]] =
      ZIO
        .hAttempt {
          config.passwordMap.map { passwordMap =>
            new Authenticator {
              override protected def getPasswordAuthentication: PasswordAuthentication = {
                val res =
                  passwordMap.get(email.from) match {
                    case Some(password) => new PasswordAuthentication(email.from.unwrap, password)
                    case None           => null
                  }
                println(res)
                res
              }
            }
          }
        }
        .mapError(HError.InternalDefect("Unable to create authenticator", _))

    private def makeSession(authenticator: Option[Authenticator]): HTask[Session] =
      ZIO
        .hAttempt {
          authenticator match {
            case Some(authenticator) => Session.getInstance(props, authenticator)
            case None                => Session.getInstance(props)
          }
        }
        .mapError(HError.InternalDefect("Error creating session", _))

    private def makeMessage(session: Session): HTask[MimeMessage] =
      ZIO.hAttempt { new MimeMessage(session) }.mapError(HError.InternalDefect("Error creating message", _))

    private def configureMessage(message: MimeMessage, email: SendEmail): HTask[Unit] =
      ZIO
        .hAttempt {
          message.setFrom(email.from.toJava)
          email.recipients.toList.foreach { recipient =>
            message.addRecipient(recipient.recipientType.java, recipient.address.toJava)
          }
          message.setSubject(email.subject)
          message.setText(email.body)
        }
        .mapError(HError.InternalDefect("Error setting message properties", _))

    private def sendMessage(message: MimeMessage): HTask[Unit] =
      ZIO.hAttempt { Transport.send(message) }.mapError(HError.InternalDefect("Unable to send email", _))

  }

  // =====| Logging Impl |=====

  val logLayer: ULayer[EmailClient] =
    ZLayer.succeed(Log)

  object Log extends EmailClient {

    override def sendEmail(email: SendEmail): HRIO[Logger & Telemetry, Unit] =
      Logger.log.info(s"EmailClient.Log.sendEmail: ${email.toJson}")

  }

}
