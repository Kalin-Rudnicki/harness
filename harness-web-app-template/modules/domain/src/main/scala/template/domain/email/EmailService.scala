package template.domain.email

import harness.email.*
import template.domain.model.*
import zio.*

trait EmailService {

  def sendEmail(
      recipient0: SendEmail.Recipient,
      recipientN: SendEmail.Recipient*,
  )(
      subject: String,
      body: String,
  ): IO[DomainError.FailedToSendEmail, Unit]

}
