package harness.archive.domain.email

import harness.email.*
import harness.zio.*
import harness.archive.domain.model.*
import zio.*

trait EmailService {

  def sendEmail(
      recipient0: SendEmail.Recipient,
      recipientN: SendEmail.Recipient*,
  )(
      subject: String,
      body: String,
  ): ZIO[Logger & Telemetry, DomainError.FailedToSendEmail, Unit]

}
