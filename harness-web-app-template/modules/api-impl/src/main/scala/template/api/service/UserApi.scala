package template.api.service

import cats.syntax.option.*
import harness.email.SendEmail
import harness.zio.*
import org.mindrot.jbcrypt.BCrypt
import template.api.model as Api
import template.domain.email.*
import template.domain.model.*
import template.domain.session.*
import template.domain.storage.*
import zio.*

final case class UserApi(
    sessionService: SessionService,
    userStorage: UserStorage,
    sessionStorage: SessionStorage,
    emailService: EmailService,
) {

  def get(token: Api.user.UserToken): IO[DomainError, User] =
    sessionService.getUserAllowUnverifiedEmail(token)

  def login(req: Api.user.Login): IO[DomainError, (User, Api.user.UserToken)] =
    for {
      _ <- Logger.log.info("Attempting login", "username" -> req.username.toLowerCase)
      user <- userStorage.byUsername(req.username).someOrFail(DomainError.InvalidUsername(req.username))
      _ <- ZIO.fail(DomainError.InvalidPassword).unless(BCrypt.checkpw(req.password, user.encryptedPassword))
      session = Session.forUser(user)
      _ <- sessionStorage.insert(session)
    } yield (user, session.token)

  def logOut(token: Api.user.UserToken): IO[DomainError, Unit] =
    for {
      _ <- Logger.log.info("Attempting logout")
      session <- sessionService.getSession(token)
      _ <- sessionStorage.deleteById(session.id)
    } yield ()

  def signUp(req: Api.user.SignUp): IO[DomainError, (User, Api.user.UserToken)] =
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

  def verifyEmail(token: Api.user.UserToken, code: Api.user.EmailVerificationCode): IO[DomainError, Unit] =
    for {
      _ <- Logger.log.info("Attempting to verify email")
      user <- sessionService.getUserAllowUnverifiedEmail(token)
      validCodes <- user.verificationEmailCodes match {
        case Some(validCodes) => ZIO.succeed(validCodes)
        case None             => ZIO.fail(DomainError.EmailAlreadyVerified(user.email))
      }
      _ <- ZIO.fail(DomainError.InvalidEmailVerificationCode(user.id, code)).unless(validCodes.contains(code))
      _ <- userStorage.setEmailCodes(user.id, None)
    } yield ()

  def resendEmailVerification(token: Api.user.UserToken): IO[DomainError, Unit] =
    for {
      _ <- Logger.log.info("Attempting to resend email code")
      user <- sessionService.getUserAllowUnverifiedEmail(token)
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

  val layer: URLayer[UserStorage & SessionStorage & EmailService & SessionService, UserApi] =
    ZLayer.fromFunction { UserApi.apply }

}
