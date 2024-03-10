package harness.archive.ui.web.helpers

import harness.http.client.{HttpClient, HttpRequest}
import harness.payments.*
import harness.payments.model.ids.*
import harness.webUI.*
import harness.zio.*
import harness.archive.api.model.error.ApiError
import harness.archive.api.model as ApiModel
import zio.*

object Api {

  type ReqIO[+A] = ZIO[HarnessEnv & HttpClient.ClientT, ApiError, A]

  object user {

    def fromSessionToken: ReqIO[ApiModel.user.User] =
      HttpRequest
        .get("/api/user/from-session-token")
        .withNoBody
        .response
        .withError[ApiError]
        .jsonBody[ApiModel.user.User]

    def fromSessionTokenOptional: ReqIO[Option[ApiModel.user.User]] =
      HttpRequest
        .get("/api/user/from-session-token-optional")
        .withNoBody
        .response
        .withError[ApiError]
        .jsonBody[Option[ApiModel.user.User]]

    def fromSessionTokenOrRedirectToLoginAllowUnverifiedEmail: ReqIO[ApiModel.user.User] =
      fromSessionTokenOptional.someOrFail(ApiError.MissingSessionToken)
    def fromSessionTokenOrRedirectToLogin: ReqIO[ApiModel.user.User] =
      fromSessionTokenOrRedirectToLoginAllowUnverifiedEmail
        .tap { user => ZIO.fail(ApiError.EmailNotVerified).unless(user.emailIsVerified) }

    def redirectToHomeIfLoggedIn: PageLoadTask[Unit] =
      fromSessionTokenOptional.toPageLoadTask.flatMap {
        case Some(_) => ZIO.fail(redirectToLogin)
        case None    => ZIO.unit
      }

    def signUp(d: ApiModel.user.SignUp): ReqIO[Unit] =
      HttpRequest
        .post("/api/user/sign-up")
        .withBodyJsonEncoded(d)
        .response
        .withError[ApiError]
        .unit2xx

    def login(d: ApiModel.user.Login): ReqIO[Unit] =
      HttpRequest
        .post("/api/user/login")
        .withBodyJsonEncoded(d)
        .response
        .withError[ApiError]
        .unit2xx

    def logOut: ReqIO[Unit] =
      HttpRequest
        .post("/api/user/log-out")
        .withNoBody
        .response
        .withError[ApiError]
        .unit2xx

    def verifyEmail(code: ApiModel.user.EmailVerificationCode): ReqIO[Unit] =
      HttpRequest
        .post("/api/user/verify-email")
        .withQueryParamEncoded("code", code)
        .withNoBody
        .response
        .withError[ApiError]
        .unit2xx

    def resendEmailCode: ReqIO[Unit] =
      HttpRequest
        .post("/api/user/resend-email-code")
        .withNoBody
        .response
        .withError[ApiError]
        .unit2xx

  }
  object payment {

    def createIntent: ReqIO[ClientSecret] =
      HttpRequest
        .post("/api/payment/create-intent")
        .withNoBody
        .response
        .withError[ApiError]
        .jsonBody[ClientSecret]

    def paymentMethods: ReqIO[Chunk[ApiModel.paymentMethod.PaymentMethod]] =
      HttpRequest
        .get("/api/payment/payment-methods")
        .withNoBody
        .response
        .withError[ApiError]
        .jsonBody[Chunk[ApiModel.paymentMethod.PaymentMethod]]

  }

}
