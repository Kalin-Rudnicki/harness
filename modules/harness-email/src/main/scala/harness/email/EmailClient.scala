package harness.email

import harness.zio.*
import java.util.Properties
import javax.mail.*
import javax.mail.internet.MimeMessage
import zio.*
import zio.json.*

trait EmailClient {
  def sendEmail(email: SendEmail): RIO[Logger & Telemetry, Unit]
}
object EmailClient {

  def sendEmail(email: SendEmail): RIO[EmailClient & Logger & Telemetry, Unit] =
    ZIO.serviceWithZIO[EmailClient](_.sendEmail(email))

  // =====| Live |=====

  val liveLayer: URLayer[EmailConfig & Logger, EmailClient] =
    ZLayer.fromZIO {
      ZIO.serviceWithZIO[EmailConfig] { config =>
        Logger.log.warning("Email config does not have auth enabled").when(config.passwordMap.isEmpty) *>
          ZIO.succeed { Live(config) }
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

    override def sendEmail(email: SendEmail): RIO[Logger & Telemetry, Unit] =
      for {
        _ <- Logger.log.debug(s"EmailClient.Live.sendEmail: ${email.toJson}")
        authenticator <- makeAuthenticator(email)
        session <- makeSession(authenticator)
        message <- makeMessage(session)
        _ <- configureMessage(message, email)
        _ <- sendMessage(message)
      } yield ()

    // =====| helpers |=====

    private def makeAuthenticator(email: SendEmail): Task[Option[Authenticator]] =
      ZIO.attempt {
        config.passwordMap.map { passwordMap =>
          new Authenticator {
            override protected def getPasswordAuthentication: PasswordAuthentication =
              passwordMap.get(email.from) match {
                case Some(password) => new PasswordAuthentication(email.from.unwrap, password)
                case None           => null
              }
          }
        }
      }

    private def makeSession(authenticator: Option[Authenticator]): Task[Session] =
      ZIO.attempt {
        authenticator match {
          case Some(authenticator) => Session.getInstance(props, authenticator)
          case None                => Session.getInstance(props)
        }
      }

    private def makeMessage(session: Session): Task[MimeMessage] =
      ZIO.attempt { new MimeMessage(session) }

    private def configureMessage(message: MimeMessage, email: SendEmail): Task[Unit] =
      ZIO.attempt {
        message.setFrom(email.from.toJava)
        email.recipients.toList.foreach { recipient =>
          message.addRecipient(recipient.recipientType.java, recipient.address.toJava)
        }
        message.setSubject(email.subject)
        message.setText(email.body)
      }

    private def sendMessage(message: MimeMessage): Task[Unit] =
      ZIO.attempt { Transport.send(message) }

  }

  // =====| Logging Impl |=====

  val logLayer: ULayer[EmailClient] =
    ZLayer.succeed(Log)

  object Log extends EmailClient {

    override def sendEmail(email: SendEmail): RIO[Logger & Telemetry, Unit] =
      Logger.log.info(s"EmailClient.Log.sendEmail: ${email.toJson}")

  }

}
