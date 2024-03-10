package template.webServer.api

import cats.syntax.option.*
import harness.email.SendEmail
import harness.zio.*
import org.mindrot.jbcrypt.BCrypt
import template.api.model as Api
import template.domain.email.*
import template.domain.model.*
import template.domain.storage.*
import zio.*

final case class UserApi(
    userStorage: UserStorage,
    sessionStorage: SessionStorage,
    emailService: EmailService,
) {

  def fromSessionToken(token: Api.user.UserToken): ZIO[HarnessEnv, DomainError, User] =
    SessionUtils.userFromSessionTokenAllowUnverifiedEmail(token, sessionStorage)

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
      emailVerificationCode = Api.user.EmailVerificationCode.gen
      user = User.fromSignUp(req, emailVerificationCode)
      session = Session.forUser(user)
      _ <- Logger.log.info("Creating user", "username" -> user.lowerUsername, "id" -> user.id)
      _ <- userStorage.insert(user)
      _ <- sessionStorage.insert(session)
      _ <- emailService.sendEmail(SendEmail.Recipient.to(user.email))(
        // TODO (KR) : update copy
        subject = "Welcome to the Template App, please verify your email address",
        body = s"Your code: $emailVerificationCode",
      )
    } yield (user, session.token)

  def verifyEmail(token: Api.user.UserToken, code: Api.user.EmailVerificationCode): ZIO[HarnessEnv, DomainError, Unit] =
    for {
      _ <- Logger.log.info("Attempting to verify email")
      user <- SessionUtils.userFromSessionTokenAllowUnverifiedEmail(token, sessionStorage)
      validCodes <- user.verificationEmailCodes match {
        case Some(validCodes) => ZIO.succeed(validCodes)
        case None             => ZIO.fail(DomainError.EmailAlreadyVerified(user.email))
      }
      _ <- ZIO.fail(DomainError.InvalidEmailVerificationCode(user.id, code)).unless(validCodes.contains(code))
      _ <- userStorage.setEmailCodes(user.id, None)
    } yield ()

  def resendEmailCode(token: Api.user.UserToken): ZIO[HarnessEnv, DomainError, Unit] =
    for {
      _ <- Logger.log.info("Attempting to resend email code")
      user <- SessionUtils.userFromSessionTokenAllowUnverifiedEmail(token, sessionStorage)
      validCodes <- user.verificationEmailCodes match {
        case Some(validCodes) => ZIO.succeed(validCodes)
        case None             => ZIO.fail(DomainError.EmailAlreadyVerified(user.email))
      }
      newCode = Api.user.EmailVerificationCode.gen
      _ <- userStorage.setEmailCodes(user.id, (validCodes + newCode).some)
      _ <- emailService.sendEmail(SendEmail.Recipient.to(user.email))(
        // TODO (KR) : update copy
        subject = "Welcome to the Template App, please verify your email address",
        body = s"Your new code: $newCode",
      )
    } yield ()

}
object UserApi {

  val layer: URLayer[UserStorage & SessionStorage & EmailService, UserApi] =
    ZLayer.fromFunction { UserApi.apply }

}
