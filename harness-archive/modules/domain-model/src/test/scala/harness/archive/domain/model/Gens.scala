package harness.archive.domain.model

import harness.archive.api.model as Api
import harness.email.EmailAddress
import org.mindrot.jbcrypt.BCrypt
import zio.*
import zio.test.*

object Gens {

  val genName: Gen[Sized, String] =
    Gen.stringBounded(5, 10)(Gen.alphaChar)

  val genEmail: Gen[Sized, EmailAddress] =
    for {
      a <- genName
      b <- genName
      c <- genName
    } yield EmailAddress.parseUnsafe(s"$a@$b.$c")

  val genPassword: Gen[Any, String] =
    Gen.uuid.map(_.toString)

  val genUserId: Gen[Any, Api.user.UserId] =
    Gen.fromZIO(Api.user.UserId.genZio)

  def genUserWithPassword(password: String): Gen[Sized, User] =
    for {
      id <- genUserId
      firstName <- genName
      lastName <- genName
      username <- genName
      email <- genEmail
    } yield User(
      id = id,
      firstName = firstName,
      lastName = lastName,
      username = username,
      lowerUsername = username.toLowerCase,
      encryptedPassword = BCrypt.hashpw(password, BCrypt.gensalt),
      email = email,
    )

  val genUser: Gen[Sized, User] =
    genPassword.flatMap(genUserWithPassword)

  def genSession(user: User): Gen[Any, Session] =
    Gen.fromZIO(ZIO.succeed(Session.forUser(user)))

  val genUserAndSession: Gen[Sized, (User, Session)] =
    for {
      user <- genUser
      session <- genSession(user)
    } yield (user, session)

}
