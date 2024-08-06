package template.ui.web.helpers

import harness.endpoint.spec.headerOrCookie
import harness.endpoint.typeclass.*
import harness.http.client.*
import harness.payments.*
import harness.payments.model.ids.*
import harness.webUI.*
import template.api.model.error.ApiError
import template.api.model as ApiModel
import template.api.spec as Spec
import zio.*

object Api {

  private val api: Spec.Api[EndpointSend] = EndpointSend.make("", "api" /: Spec.Api.spec(headerOrCookie.raw[ApiModel.user.UserToken]("empty")))

  type ReqIO[+A] = IO[ApiError, A]

  object user {

    def fromSessionToken: ReqIO[ApiModel.user.User] = api.user.get.withoutAuth()

    def fromSessionTokenOptional: ReqIO[Option[ApiModel.user.User]] =
      fromSessionToken.asSome.catchSome { case ApiError.MissingSessionToken => ZIO.none }

    def fromSessionTokenOrRedirectToLogin: ReqIO[ApiModel.user.User] =
      fromSessionToken
        .tap { user => ZIO.fail(ApiError.EmailNotVerified).unless(user.emailIsVerified) }

    def redirectToHomeIfLoggedIn: PageLoadTask[Unit] =
      fromSessionTokenOptional.toPageLoadTask.flatMap {
        case Some(_) => ZIO.fail(redirectToLogin)
        case None    => ZIO.unit
      }

    def signUp(d: ApiModel.user.SignUp): ReqIO[ApiModel.user.User] = api.user.signUp(d)
    def login(d: ApiModel.user.Login): ReqIO[ApiModel.user.User] = api.user.login(d)
    def logOut: ReqIO[Unit] = api.user.logOut.withoutAuth()
    def verifyEmail(code: ApiModel.user.EmailVerificationCode): ReqIO[Unit] = api.user.verifyEmail.withoutAuth(code)
    def resendEmailCode: ReqIO[Unit] = api.user.resendEmailVerification.withoutAuth()

  }
  object payment {

    def createIntent: ReqIO[ClientSecret] =
      api.payment.createIntent.withoutAuth()

    def paymentMethods: ReqIO[Chunk[ApiModel.paymentMethod.PaymentMethod]] =
      api.payment.paymentMethods.withoutAuth()

  }

}
