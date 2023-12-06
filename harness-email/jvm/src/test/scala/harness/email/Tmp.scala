package harness.email

import cats.data.NonEmptyList
import cats.syntax.option.*
import harness.zio.*
import zio.*

object Tmp extends ExecutableApp {

  override val executable: Executable =
    Executable
      .withLayer {
        ZLayer.succeed(
          EmailConfig(
            "smtp.gmail.com",
            465,
            EmailConfig.AuthType.SSL,
            Map(
              EmailAddress.parseUnsafe("harness.email.client@gmail.com") -> "cjxf zzue pwqp dunf",
            ).some,
          ),
        ) >>> EmailClient.liveLayer
      }
      .withEffect {
        for {
          _ <- Logger.log.info("Starting TMP")
          email = SendEmail(
            from = EmailAddress.parseUnsafe("harness.email.client@gmail.com"),
            recipients = NonEmptyList.of(
              SendEmail.Recipient.to(EmailAddress.parseUnsafe("kalin.rudnicki@gmail.com")),
            ),
            subject = "email subject",
            body = "email body",
          )
          _ <- EmailClient.sendEmail(email)
        } yield ()
      }

}
