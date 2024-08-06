package harness.email

import cats.syntax.either.*
import cats.syntax.option.*
import harness.zio.*
import java.util.Properties
import javax.mail.*
import javax.mail.internet.MimeMessage
import zio.*
import zio.json.*

trait EmailClient {
  def sendEmail(email: SendEmail): Task[Unit]
}
object EmailClient {

  def sendEmail(email: SendEmail): RIO[EmailClient, Unit] =
    ZIO.serviceWithZIO[EmailClient](_.sendEmail(email))

  // =====| Live |=====

  val liveLayer: URLayer[EmailConfig, EmailClient] =
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

    override def sendEmail(email: SendEmail): Task[Unit] =
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

    override def sendEmail(email: SendEmail): Task[Unit] =
      Logger.log.info(s"EmailClient.Log.sendEmail: ${email.toJson}")

  }

  // =====| Live Or Logged |=====

  sealed trait LiveOrLoggedConfig
  object LiveOrLoggedConfig {

    case object Logged extends LiveOrLoggedConfig
    final case class Live(config: EmailConfig) extends LiveOrLoggedConfig

    private final case class Raw(
        live: Option[EmailConfig],
        logged: Option[Boolean],
    ) derives JsonCodec

    implicit val jsonCodec: JsonCodec[LiveOrLoggedConfig] =
      JsonCodec[Raw].transformOrFail(
        {
          case Raw(_, Some(true)) => LiveOrLoggedConfig.Logged.asRight
          case Raw(Some(live), _) => LiveOrLoggedConfig.Live(live).asRight
          case _                  => "You must provide either `live` or `logged`".asLeft
        },
        {
          case LiveOrLoggedConfig.Logged       => Raw(None, true.some)
          case LiveOrLoggedConfig.Live(config) => Raw(config.some, None)
        },
      )

  }

  val liveOrLoggedLayer: URLayer[LiveOrLoggedConfig, EmailClient] =
    ZLayer.service[LiveOrLoggedConfig].project {
      case LiveOrLoggedConfig.Logged       => EmailClient.Log
      case LiveOrLoggedConfig.Live(config) => EmailClient.Live(config)
    }

}
