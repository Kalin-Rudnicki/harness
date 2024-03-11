package harness.archive.domain.model

import cats.syntax.option.*
import harness.archive.api.model as Api
import harness.email.EmailAddress
import org.mindrot.jbcrypt.BCrypt

final case class User(
    id: Api.user.UserId,
    firstName: String,
    lastName: String,
    username: String,
    lowerUsername: String,
    encryptedPassword: String,
    email: EmailAddress,
) { self =>

  def toApi: Api.user.User =
    Api.user.User(
      id = self.id,
      firstName = self.firstName,
      lastName = self.lastName,
      username = self.username,
      email = self.email,
    )

}
object User {

  def fromSignUp(req: Api.user.SignUp): User =
    User(
      id = Api.user.UserId.gen,
      firstName = req.firstName,
      lastName = req.lastName,
      username = req.username,
      lowerUsername = req.username.toLowerCase,
      encryptedPassword = BCrypt.hashpw(req.password, BCrypt.gensalt),
      email = req.email,
    )

}
