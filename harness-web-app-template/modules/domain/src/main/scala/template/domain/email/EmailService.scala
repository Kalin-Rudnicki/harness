package template.domain.email

import harness.email.*
import harness.zio.*
import template.domain.model.*
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
