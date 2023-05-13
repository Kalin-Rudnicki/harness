package template.api.routes

import harness.core.*
import harness.sql.*
import harness.sql.query.Transaction
import harness.web.*
import harness.http.server.{given, *}
import harness.zio.*
import org.mindrot.jbcrypt.BCrypt
import template.api.db.{model as M, queries as Q}
import template.model as D
import zio.*

object User {

  val routes: Route[JDBCConnection & Transaction] =
    "user" /: Route.oneOf(
      (HttpMethod.GET / "from-session-token").implement { _ =>
        Transaction.inTransaction {
          for {
            dbUser <- Helpers.userFromSession
          } yield HttpResponse.encodeJson(Helpers.convert.user(dbUser))
        }
      },
      (HttpMethod.GET / "from-session-token-optional").implement { _ =>
        Transaction.inTransaction {
          for {
            dbUser <- Helpers.userFromSessionOptional
            user = dbUser.map(Helpers.convert.user)
          } yield HttpResponse.encodeJson(user)
        }
      },
      (HttpMethod.POST / "login").implement { _ =>
        Transaction.inTransaction {
          for {
            body <- HttpRequest.jsonBody[D.user.Login]
            user <- Q.User.byUsername(body.username).option.someOrFail(HError.UserError(s"Invalid username: '${body.username}'"))
            _ <- ZIO.fail(HError.UserError("Invalid Password")).unless(BCrypt.checkpw(body.password, user.encryptedPassword))
            session = M.Session.newForUser(user)
            _ <- Q.Session.insert(session).single
          } yield HttpResponse.encodeJson(Helpers.convert.user(user)).withCookie(Cookie(Helpers.SessionToken, session.token).rootPath.secure)
        }
      },
      (HttpMethod.POST / "log-out").implement { _ =>
        Transaction.inTransaction {
          for {
            tok <- HttpRequest.cookie.get[String](Helpers.SessionToken)
            dbSession <- Q.Session.fromSessionToken(tok).single
            _ <- Q.Session.deleteById(dbSession.id).single
          } yield HttpResponse.fromHttpCode.Ok.withCookie(Cookie.unset(Helpers.SessionToken).rootPath.secure)
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
            _ <- Q.User.insert(user).single
            _ <- Q.Session.insert(session).single
          } yield HttpResponse.fromHttpCode.Ok.withCookie(Cookie(Helpers.SessionToken, session.token).rootPath.secure)
        }
      },
    )

}
