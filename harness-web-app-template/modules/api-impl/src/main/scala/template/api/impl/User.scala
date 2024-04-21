package template.api.impl

import harness.http.server.*
import harness.sql.query.Transaction
import template.api.service.*
import template.api.spec as Spec
import template.domain.session.SessionService
import zio.*

object User {

  val impl: Spec.User[Implementation.Projection[Api.Env]] =
    Spec.User[Implementation.Projection[Api.Env]](
      get = Implementation[Spec.User.Get].implement { token =>
        ZIO.serviceWithZIO[UserApi](_.get(token)).map(_.toApi).toHttpResponse
      },
      login = Implementation[Spec.User.Login].implement { login =>
        Transaction.inTransaction {
          for {
            (user, token) <- ZIO.serviceWithZIO[UserApi](_.login(login))
            isSecure <- ZIO.serviceWithZIO[SessionService](_.isSecure)
            tokenKey <- ZIO.serviceWithZIO[SessionService](_.tokenKey)
            cookie = SetCookie(tokenKey, token.value).rootPath.secure(isSecure)
          } yield HttpResponse(user.toApi).withHeader(tokenKey, token.value).withCookie(cookie)
        }
      },
      logOut = Implementation[Spec.User.LogOut].implement { token =>
        for {
          _ <- ZIO.serviceWithZIO[UserApi](_.logOut(token))
          isSecure <- ZIO.serviceWithZIO[SessionService](_.isSecure)
          tokenKey <- ZIO.serviceWithZIO[SessionService](_.tokenKey)
          cookie = SetCookie.unset(tokenKey).rootPath.secure(isSecure)
        } yield HttpResponse(()).withCookie(cookie)
      },
      signUp = Implementation[Spec.User.SignUp].implement { signUp =>
        Transaction.inTransaction {
          for {
            (user, token) <- ZIO.serviceWithZIO[UserApi](_.signUp(signUp))
            isSecure <- ZIO.serviceWithZIO[SessionService](_.isSecure)
            tokenKey <- ZIO.serviceWithZIO[SessionService](_.tokenKey)
            cookie = SetCookie(tokenKey, token.value).rootPath.secure(isSecure)
          } yield HttpResponse(user.toApi).withHeader(tokenKey, token.value).withCookie(cookie)
        }
      },
      verifyEmail = Implementation[Spec.User.VerifyEmail].implement { (code, token) =>
        ZIO.serviceWithZIO[UserApi](_.verifyEmail(token, code)).toHttpResponse
      },
      resendEmailVerification = Implementation[Spec.User.ResendEmailVerification].implement { token =>
        ZIO.serviceWithZIO[UserApi](_.resendEmailVerification(token)).toHttpResponse
      },
    )

}
