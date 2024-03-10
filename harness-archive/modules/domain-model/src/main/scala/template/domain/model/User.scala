package template.domain.model

import cats.syntax.option.*
import harness.email.EmailAddress
import harness.payments.model.ids as StripeIds
import org.mindrot.jbcrypt.BCrypt
import template.api.model as Api

final case class User(
    id: Api.user.UserId,
    firstName: String,
    lastName: String,
    username: String,
    lowerUsername: String,
    encryptedPassword: String,
    email: EmailAddress,
    verificationEmailCodes: Option[Set[Api.user.EmailVerificationCode]],
    stripeCustomerId: Option[StripeIds.CustomerId],
) { self =>

  def toApi: Api.user.User =
    Api.user.User(
      id = self.id,
      firstName = self.firstName,
      lastName = self.lastName,
      username = self.username,
      email = self.email,
      emailIsVerified = self.verificationEmailCodes.isEmpty,
    )

}
object User {

  def fromSignUp(req: Api.user.SignUp, code: Api.user.EmailVerificationCode): User =
    User(
      id = Api.user.UserId.gen,
      firstName = req.firstName,
      lastName = req.lastName,
      username = req.username,
      lowerUsername = req.username.toLowerCase,
      encryptedPassword = BCrypt.hashpw(req.password, BCrypt.gensalt),
      email = req.email,
      verificationEmailCodes = Set(code).some,
      stripeCustomerId = None,
    )

}
