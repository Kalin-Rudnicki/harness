package template.ui.web.helpers

import harness.http.client.{HttpClient, HttpRequest}
import harness.payments.*
import harness.payments.model.ids.*
import harness.webUI.*
import harness.zio.*
import template.model as D
import zio.*

object Api {

  type ReqIO[T] = HRIO[HttpClient.ClientT & Logger & Telemetry, T]

  object user {

    def fromSessionToken: ReqIO[D.user.User] =
      HttpRequest
        .get("/api/user/from-session-token")
        .withNoBody
        .response
        .jsonBody[D.user.User]

    def fromSessionTokenOptional: ReqIO[Option[D.user.User]] =
      HttpRequest
        .get("/api/user/from-session-token-optional")
        .withNoBody
        .response
        .jsonBody[Option[D.user.User]]

    def fromSessionTokenOrRedirectToLogin(allowUnverifiedEmail: Boolean): ReqIO[D.user.User] =
      fromSessionTokenOptional.flatMap {
        case Some(user) if !user.emailIsVerified && !allowUnverifiedEmail => ZIO.fail(Page.PageLoadRedirect(Url("page", "verify-email")()))
        case Some(user)                                                   => ZIO.succeed(user)
        case None                                                         => ZIO.fail(Page.PageLoadRedirect(Url("page", "login")()))
      }
    def fromSessionTokenOrRedirectToLogin: ReqIO[D.user.User] =
      fromSessionTokenOrRedirectToLogin(false)

    def redirectToHomeIfLoggedIn: ReqIO[Unit] =
      fromSessionTokenOptional.flatMap {
        case Some(_) => ZIO.fail(Page.PageLoadRedirect(Url("page", "home")()))
        case None    => ZIO.unit
      }

    def signUp(d: D.user.SignUp): ReqIO[Unit] =
      HttpRequest
        .post("/api/user/sign-up")
        .withBodyJsonEncoded(d)
        .response
        .unit2xx

    def login(d: D.user.Login): ReqIO[Unit] =
      HttpRequest
        .post("/api/user/login")
        .withBodyJsonEncoded(d)
        .response
        .unit2xx

    def logOut: ReqIO[Unit] =
      HttpRequest
        .post("/api/user/log-out")
        .withNoBody
        .response
        .unit2xx

    def verifyEmail(code: D.user.EmailVerificationCode): ReqIO[Unit] =
      HttpRequest
        .post("/api/user/verify-email")
        .withQueryParamEncoded("code", code)
        .withNoBody
        .response
        .unit2xx

    def resendEmailCode: ReqIO[Unit] =
      HttpRequest
        .post("/api/user/resend-email-code")
        .withNoBody
        .response
        .unit2xx

  }
  object payment {

    def createIntent: ReqIO[ClientSecret] =
      HttpRequest
        .post("/api/payment/create-intent")
        .withNoBody
        .response
        .jsonBody[ClientSecret]

    def paymentMethods: ReqIO[Chunk[D.paymentMethod.PaymentMethod]] =
      HttpRequest
        .get("/api/payment/payment-methods")
        .withNoBody
        .response
        .jsonBody[Chunk[D.paymentMethod.PaymentMethod]]

  }

}
