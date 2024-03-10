package harness.archive.domain.impl.email

import cats.data.NonEmptyList
import harness.email.*
import harness.zio.*
import harness.archive.domain.email.EmailService
import harness.archive.domain.model.DomainError
import zio.*
import zio.json.*

final case class LiveEmailService(
    config: LiveEmailService.Config,
    client: EmailClient,
) extends EmailService {

  override def sendEmail(
      recipient0: SendEmail.Recipient,
      recipientN: SendEmail.Recipient*,
  )(
      subject: String,
      body: String,
  ): ZIO[Logger & Telemetry, DomainError.FailedToSendEmail, Unit] =
    client
      .sendEmail(
        SendEmail(
          from = config.sender,
          recipients = NonEmptyList(recipient0, recipientN.toList),
          subject = subject,
          body = body,
        ),
      )
      .mapError(DomainError.FailedToSendEmail(_))

}
object LiveEmailService {

  final case class Config(
      sender: EmailAddress,
  )
  object Config {
    implicit val jsonCodec: JsonCodec[Config] = DeriveJsonCodec.gen
  }

  val liveLayer: URLayer[LiveEmailService.Config & EmailClient, EmailService] =
    ZLayer.fromFunction { LiveEmailService.apply }

  val logLayer: ULayer[EmailService] =
    ZLayer.make[EmailService](
      ZLayer.succeed(LiveEmailService.Config(EmailAddress.parseUnsafe("no.op@email.com"))),
      EmailClient.logLayer,
      LiveEmailService.liveLayer,
    )

}
