package harness.archive.api.routes

import harness.archive.api.db.model as M
import harness.archive.api.service.storage.*
import harness.archive.api.util.*
import harness.archive.api.util.SessionUtils.isSecure
import harness.archive.model as D
import harness.core.*
import harness.http.server.{given, *}
import harness.sql.*
import harness.sql.query.Transaction
import harness.web.*
import harness.zio.*
import org.mindrot.jbcrypt.BCrypt
import zio.*

object User {

  val routes: Route[UserStorage & SessionStorage & Transaction] =
    "user" /: Route.oneOf(
      (HttpMethod.GET / "from-session-token").implement { _ =>
        Transaction.inTransaction {
          for {
            dbUser <- SessionUtils.userFromSessionToken
          } yield HttpResponse.encodeJson(DbToDomain.user(dbUser))
        }
      },
      (HttpMethod.GET / "from-session-token-optional").implement { _ =>
        Transaction.inTransaction {
          for {
            dbUser <- SessionUtils.userFromSessionTokenOptional
          } yield HttpResponse.encodeJson(dbUser.map(DbToDomain.user))
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
            user = new M.User.Identity(M.User.Id.gen, body.firstName, body.lastName, body.username, body.username.toLowerCase, encryptedPassword, body.email)
            session = M.Session.newForUser(user)
            _ <- Logger.log.detailed(s"Creating user ${user.show}")
            _ <- UserStorage.insert(user)
            _ <- SessionStorage.insert(session)
          } yield HttpResponse.fromHttpCode.Ok.withCookie(Cookie(SessionUtils.SessionToken, session.token).rootPath.secure(isSecure))
        }
      },
    )

}
