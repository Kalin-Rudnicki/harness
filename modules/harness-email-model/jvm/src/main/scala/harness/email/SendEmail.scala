package harness.email

import cats.data.NonEmptyList
import harness.core.Enum
import harness.schema.*
import harness.zio.*
import harness.zio.json.*
import javax.mail.Message.RecipientType as JavaRecipientType
import zio.json.*

final case class SendEmail(
    from: EmailAddress,
    recipients: NonEmptyList[SendEmail.Recipient],
    subject: String,
    body: String,
)
object SendEmail {

  def simple(from: EmailAddress, to: EmailAddress, subject: String, body: String): SendEmail =
    SendEmail(from, NonEmptyList.one(Recipient.to(to)), subject, body)

  enum RecipientType(final val java: JavaRecipientType) extends Enum[RecipientType] {
    case To extends RecipientType(JavaRecipientType.TO)
    case CC extends RecipientType(JavaRecipientType.CC)
    case BCC extends RecipientType(JavaRecipientType.BCC)
  }
  object RecipientType extends Enum.Companion[RecipientType]

  final case class Recipient(
      recipientType: RecipientType,
      address: EmailAddress,
  )
  object Recipient {

    def to(address: EmailAddress): Recipient = Recipient(RecipientType.To, address)
    def cc(address: EmailAddress): Recipient = Recipient(RecipientType.CC, address)
    def bcc(address: EmailAddress): Recipient = Recipient(RecipientType.BCC, address)

    implicit val schema: JsonSchema[Recipient] = JsonSchema.derived

  }

  implicit val schema: JsonSchema[SendEmail] = JsonSchema.derived

}
