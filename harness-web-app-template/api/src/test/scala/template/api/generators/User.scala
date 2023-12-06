package template.api.generators

import harness.email.EmailAddress
import harness.http.server.HttpRequest
import harness.http.server.test.*
import harness.sql.*
import harness.sql.query.Transaction
import harness.zio.*
import org.mindrot.jbcrypt.BCrypt
import template.api.*
import template.api.db.model as M
import template.api.service.storage.*
import template.model as D
import zio.*
import zio.test.*

object User {

  val signUpGen: Gen[Sized, D.user.SignUp] =
    for {
      firstName <- Common.nameGen
      lastName <- Common.nameGen
      username <- Common.nameGen
      password <- Gen.alphaNumericStringBounded(10, 20)
      domain <- Common.nameGen
    } yield D.user.SignUp(
      firstName = firstName,
      lastName = lastName,
      username = username,
      password = password,
      email = EmailAddress.parseUnsafe(s"${firstName.toLowerCase}.${lastName.toLowerCase}@${domain.toLowerCase}.com"),
    )

  val insertedUserGen: Gen[UserStorage & Logger & Telemetry & Sized, D.user.User] =
    signUpGen.flatMap { signUp =>
      Gen.fromZIO {
        val encryptedPassword = BCrypt.hashpw(signUp.password, BCrypt.gensalt)
        val user = new M.User.Identity(M.User.Id.gen, signUp.firstName, signUp.lastName, signUp.username, signUp.username.toLowerCase, encryptedPassword, signUp.email)
        UserStorage.insert(user).as(D.user.User(user.id, user.firstName, user.lastName, user.username, user.email)).orDie
      }
    }

  def signedInUserGen[SE <: JDBCConnectionPool, RE <: Transaction](routeSpec: RouteSpec[SE, RE]): Gen[routeSpec.HttpEnv & Sized, D.user.User] =
    for {
      signUp <- signUpGen
      _ <- Gen.fromZIO { routeSpec.httpRequest(HttpRequest.builder.post("api", "user", "sign-up").jsonBody(signUp)).orDie }
      user <- Gen.fromZIO { routeSpec.httpRequest.jsonResponse[D.user.User](HttpRequest.builder.get("api", "user", "from-session-token").noBody).orDie }
    } yield user

}
