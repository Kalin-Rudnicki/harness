package harness.archive.webServer.api

import cats.syntax.option.*
import harness.archive.api.model as Api
import harness.archive.domain.model.*
import harness.archive.domain.storage.*
import harness.zio.*
import org.mindrot.jbcrypt.BCrypt
import zio.*

final case class UserApi(
    userStorage: UserStorage,
    sessionStorage: SessionStorage,
) {

  def fromSessionToken(token: Api.user.UserToken): ZIO[HarnessEnv, DomainError, User] =
    SessionUtils.userFromSessionToken(token, sessionStorage)

  def fromSessionTokenOptional(token: Option[Api.user.UserToken]): ZIO[HarnessEnv, DomainError, Option[User]] =
    ZIO.foreach(token)(fromSessionToken)

  def login(req: Api.user.Login): ZIO[HarnessEnv, DomainError, (User, Api.user.UserToken)] =
    for {
      _ <- Logger.log.info("Attempting login", "username" -> req.username.toLowerCase)
      user <- userStorage.byUsername(req.username).someOrFail(DomainError.InvalidUsername(req.username))
      _ <- ZIO.fail(DomainError.InvalidPassword).unless(BCrypt.checkpw(req.password, user.encryptedPassword))
      session = Session.forUser(user)
      _ <- sessionStorage.insert(session)
    } yield (user, session.token)

  def logOut(token: Api.user.UserToken): ZIO[HarnessEnv, DomainError, Unit] =
    for {
      _ <- Logger.log.info("Attempting logout")
      session <- SessionUtils.sessionFromSessionToken(token, sessionStorage)
      _ <- sessionStorage.deleteById(session.id)
    } yield ()

  def signUp(req: Api.user.SignUp): ZIO[HarnessEnv, DomainError, (User, Api.user.UserToken)] =
    for {
      _ <- Logger.log.info("Attempting sign-up", "username" -> req.username.toLowerCase)
      existingUser <- userStorage.byUsername(req.username)
      _ <- ZIO.fail(DomainError.UsernameAlreadyExists(req.username.toLowerCase)).when(existingUser.nonEmpty)
      user = User.fromSignUp(req)
      session = Session.forUser(user)
      _ <- Logger.log.info("Creating user", "username" -> user.lowerUsername, "id" -> user.id)
      _ <- userStorage.insert(user)
      _ <- sessionStorage.insert(session)
    } yield (user, session.token)

}
object UserApi {

  val layer: URLayer[UserStorage & SessionStorage, UserApi] =
    ZLayer.fromFunction { UserApi.apply }

}
