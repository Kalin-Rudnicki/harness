package harness.email

import cats.syntax.either.*
import harness.core.*
import zio.json.*

final case class EmailAddress private (unwrap: String) { self =>
  def toLowerEmail: EmailAddress = EmailAddress.parseUnsafe(self.unwrap.toLowerCase)
}
object EmailAddress {

  private val regex = """^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,}$""".r

  def parse(email: String): Either[String, EmailAddress] =
    if (regex.matches(email)) EmailAddress(email).asRight
    else s"Invalid email address: $email".asLeft
  def parseUnsafe(email: String): EmailAddress =
    EmailAddress.parse(email) match {
      case Right(email) => email
      case Left(error)  => throw new RuntimeException(error)
    }

  implicit val stringEncoder: StringEncoder[EmailAddress] = StringEncoder.usingToString
  implicit val stringDecoder: StringDecoder[EmailAddress] = StringDecoder.fromEitherF(EmailAddress.parse)

  implicit val jsonFieldEncoder: JsonFieldEncoder[EmailAddress] = JsonFieldEncoder.string.contramap(_.unwrap)
  implicit val jsonFieldDecoder: JsonFieldDecoder[EmailAddress] = JsonFieldDecoder.string.mapOrFail(EmailAddress.parse)

}
