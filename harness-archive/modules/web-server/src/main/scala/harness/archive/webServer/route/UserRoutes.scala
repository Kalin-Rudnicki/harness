package harness.archive.webServer.route

import harness.http.server.{given, *}
import harness.sql.query.Transaction
import harness.web.*
import harness.archive.api.model as Api
import harness.archive.domain.impl.storage.postgres.StorageUtils.errorMapper
import harness.archive.webServer.api.*
import harness.archive.webServer.route.RouteUtils.*
import zio.*

object UserRoutes {

  val routes: Route[UserApi & SessionConfig & Transaction] =
    "user" /: Route.oneOf(
      (HttpMethod.GET / "from-session-token").implement { _ =>
        for {
          token <- sessionToken
          user <- ZIO.serviceWithZIO[UserApi](_.fromSessionToken(token))
        } yield HttpResponse.encodeJson(user.toApi)
      },
      (HttpMethod.GET / "from-session-token-optional").implement { _ =>
        for {
          optToken <- sessionTokenOptional
          optUser <- ZIO.serviceWithZIO[UserApi](_.fromSessionTokenOptional(optToken))
        } yield HttpResponse.encodeJson(optUser.map(_.toApi))
      },
      (HttpMethod.POST / "login").implement { _ =>
        Transaction.inTransaction {
          for {
            req <- HttpRequest.jsonBody[Api.user.Login].mapError(_.toDomain)
            (user, token) <- ZIO.serviceWithZIO[UserApi](_.login(req))
            sessionConfig <- ZIO.service[SessionConfig]
          } yield HttpResponse.encodeJson(user.toApi).withCookie(Cookie(sessionConfig.key, token.value).rootPath.secure(sessionConfig.isSecure))
        }
      },
      (HttpMethod.POST / "log-out").implement { _ =>
        for {
          token <- sessionToken
          _ <- ZIO.serviceWithZIO[UserApi](_.logOut(token))
          sessionConfig <- ZIO.service[SessionConfig]
        } yield HttpResponse.fromHttpCode.Ok.withCookie(Cookie.unset(sessionConfig.key).rootPath.secure(sessionConfig.isSecure))
      },
      (HttpMethod.POST / "sign-up").implement { _ =>
        Transaction.inTransaction {
          for {
            req <- HttpRequest.jsonBody[Api.user.SignUp].mapError(_.toDomain)
            (user, token) <- ZIO.serviceWithZIO[UserApi](_.signUp(req))
            sessionConfig <- ZIO.service[SessionConfig]
          } yield HttpResponse.encodeJson(user.toApi).withCookie(Cookie(sessionConfig.key, token.value).rootPath.secure(sessionConfig.isSecure))
        }
      },
    )

}
