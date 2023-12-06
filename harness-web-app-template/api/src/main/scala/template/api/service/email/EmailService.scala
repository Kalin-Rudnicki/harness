package template.api.service.email

import cats.data.NonEmptyList
import harness.email.*
import harness.zio.*
import zio.*
import zio.json.*

final case class EmailService(
    config: EmailService.Config,
    client: EmailClient,
) {

  def sendEmail(
      recipient0: SendEmail.Recipient,
      recipientN: SendEmail.Recipient*,
  )(
      subject: String,
      body: String,
  ): HRIO[Logger & Telemetry, Unit] =
    client.sendEmail(
      SendEmail(
        from = config.sender,
        recipients = NonEmptyList(recipient0, recipientN.toList),
        subject = subject,
        body = body,
      ),
    )

}
object EmailService {

  final case class Config(
      sender: EmailAddress,
  )
  object Config {
    implicit val jsonCodec: JsonCodec[Config] = DeriveJsonCodec.gen
  }

  val liveLayer: URLayer[EmailService.Config & EmailClient, EmailService] =
    ZLayer.fromFunction { EmailService.apply }

  val logLayer: ULayer[EmailService] =
    ZLayer.make[EmailService](
      ZLayer.succeed(EmailService.Config(EmailAddress.parseUnsafe("no.op@email.com"))),
      EmailClient.logLayer,
      EmailService.liveLayer,
    )

  def sendEmail(
      recipient0: SendEmail.Recipient,
      recipientN: SendEmail.Recipient*,
  )(
      subject: String,
      body: String,
  ): HRIO[EmailService & Logger & Telemetry, Unit] =
    ZIO.serviceWithZIO[EmailService](_.sendEmail(recipient0, recipientN*)(subject, body))

}
