package template.webServer.route

import harness.http.server.{given, *}
import harness.sql.query.Transaction
import harness.web.*
import template.api.model as Api
import template.domain.model.DomainError
import template.webServer.api.*
import template.webServer.route.RouteUtils.*
import zio.*

object UserRoutes {

  val routes: Route[UserApi & SessionConfig & Transaction[DomainError]] =
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
      (HttpMethod.POST / "verify-email").implement { _ =>
        for {
          token <- sessionToken
          code <- HttpRequest.query.get[Api.user.EmailVerificationCode]("code").mapError(_.toDomain)
          _ <- ZIO.serviceWithZIO[UserApi](_.verifyEmail(token, code))
        } yield HttpResponse.fromHttpCode.Ok
      },
      (HttpMethod.POST / "resend-email-code").implement { _ =>
        for {
          token <- sessionToken
          _ <- ZIO.serviceWithZIO[UserApi](_.resendEmailCode(token))
        } yield HttpResponse.fromHttpCode.Ok
      },
    )

}
