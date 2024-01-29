package template.api.routes

import cats.syntax.option.*
import harness.core.*
import harness.email.*
import harness.http.server.{given, *}
import harness.sql.*
import harness.sql.query.Transaction
import harness.web.*
import harness.zio.*
import org.mindrot.jbcrypt.BCrypt
import template.api.db.model as M
import template.api.service.email.*
import template.api.service.storage.*
import template.api.util.*
import template.api.util.SessionUtils.isSecure
import template.model as D
import zio.*

object User {

  val routes: Route[UserStorage & SessionStorage & Transaction & EmailService] =
    "user" /: Route.oneOf(
      (HttpMethod.GET / "from-session-token").implement { _ =>
        Transaction.inTransaction {
          for {
            dbUser <- SessionUtils.userFromSessionToken(true)
          } yield HttpResponse.encodeJson(DbToDomain.user(dbUser))
        }
      },
      (HttpMethod.GET / "from-session-token-optional").implement { _ =>
        Transaction.inTransaction {
          for {
            dbUser <- SessionUtils.userFromSessionTokenOptional
            user = dbUser.map(DbToDomain.user)
          } yield HttpResponse.encodeJson(user)
        }
      },
      (HttpMethod.POST / "login").implement { _ =>
        Transaction.inTransaction {
          for {
            body <- HttpRequest.jsonBody[D.user.Login]
            user <- UserStorage.byUsername(body.username).someOrFail(HError.UserError(s"Invalid username: '${body.username}'"))
            _ <- ZIO.fail(HError.UserError("Invalid Password")).unless(BCrypt.checkpw(body.password, user.encryptedPassword))
            session = M.Session.newForUser(user)
            _ <- SessionStorage.insert(session)
          } yield HttpResponse.encodeJson(DbToDomain.user(user)).withCookie(Cookie(SessionUtils.SessionToken, session.token).rootPath.secure(isSecure))
        }
      },
      (HttpMethod.POST / "log-out").implement { _ =>
        Transaction.inTransaction {
          for {
            dbSession <- SessionUtils.sessionFromSessionToken
            _ <- SessionStorage.deleteById(dbSession.id)
          } yield HttpResponse.fromHttpCode.Ok.withCookie(Cookie.unset(SessionUtils.SessionToken).rootPath.secure(isSecure))
        }
      },
      (HttpMethod.POST / "sign-up").implement { _ =>
        Transaction.inTransaction {
          for {
            body <- HttpRequest.jsonBody[D.user.SignUp]
            encryptedPassword = BCrypt.hashpw(body.password, BCrypt.gensalt)
            emailVerificationCode = D.user.EmailVerificationCode.gen
            user = new M.User.Identity(M.User.Id.gen, body.firstName, body.lastName, body.username, body.username.toLowerCase, encryptedPassword, body.email, Set(emailVerificationCode).some, None)
            session = M.Session.newForUser(user)
            _ <- Logger.log.detailed(s"Creating user ${user.show}")
            _ <- UserStorage.insert(user)
            _ <- SessionStorage.insert(session)

            _ <- EmailService.sendEmail(SendEmail.Recipient.to(user.email))(
              // TODO (KR) :
              subject = "Welcome to the Template App, please verify your email address",
              body = s"Your code: $emailVerificationCode",
            )
          } yield HttpResponse.fromHttpCode.Ok.withCookie(Cookie(SessionUtils.SessionToken, session.token).rootPath.secure(isSecure))
        }
      },
      (HttpMethod.POST / "verify-email").implement { _ =>
        Transaction.inTransaction {
          for {
            dbUser <- SessionUtils.userFromSessionToken(true)
            code <- HttpRequest.query.get[D.user.EmailVerificationCode]("code")
            validCodes <- dbUser.verificationEmailCodes match {
              case Some(codes) => ZIO.succeed(codes)
              case None        => ZIO.fail(HError.UserError("You have already verified your email address"))
            }
            _ <- ZIO.fail(HError.UserError("Invalid verification code")).unless(validCodes.contains(code))
            _ <- UserStorage.setEmailCodes(dbUser.id, None)
          } yield HttpResponse.fromHttpCode.Ok
        }
      },
      (HttpMethod.POST / "resend-email-code").implement { _ =>
        Transaction.inTransaction {
          for {
            dbUser <- SessionUtils.userFromSessionToken(true)
            validCodes <- dbUser.verificationEmailCodes match {
              case Some(codes) => ZIO.succeed(codes)
              case None        => ZIO.fail(HError.UserError("You have already verified your email address"))
            }
            newCode = D.user.EmailVerificationCode.gen
            _ <- UserStorage.setEmailCodes(dbUser.id, (validCodes + newCode).some)
            _ <- EmailService.sendEmail(SendEmail.Recipient.to(dbUser.email))(
              // TODO (KR) :
              subject = "Welcome to the Template App, please verify your email address",
              body = s"Your new code: $newCode",
            )
          } yield HttpResponse.fromHttpCode.Ok
        }
      },
    )

}
