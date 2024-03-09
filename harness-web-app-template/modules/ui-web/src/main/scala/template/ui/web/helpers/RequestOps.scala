package template.ui.web.helpers

import harness.http.client.HttpClient
import harness.webUI.*
import harness.webUI.error.UIError
import harness.zio.*
import template.api.model.error.ApiError
import zio.*

def redirectToLogin: UIError.Redirect = UIError.Redirect(Url("page", "login")())
def redirectToHome: UIError.Redirect = UIError.Redirect(Url("page", "home")())
def redirectToVerifyEmail: UIError.Redirect = UIError.Redirect(Url("page", "verify-email")())

implicit class RequestOps[A](self: ZIO[HarnessEnv & HttpClient.ClientT, ApiError, A]) {

  def toPageLoadTask: PageLoadTask[A] =
    self.mapError {
      case ApiError.MissingSessionToken          => redirectToLogin
      case ApiError.InvalidSessionToken          => redirectToLogin
      case ApiError.UsernameAlreadyExists        => UIError.Failure.error("Username already exists")
      case ApiError.InvalidLoginCredentials      => UIError.Failure.error("Invalid login credentials")
      case ApiError.EmailNotVerified             => redirectToVerifyEmail
      case ApiError.EmailAlreadyVerified         => UIError.Failure.error("Your email is already verified")
      case ApiError.InvalidEmailVerificationCode => UIError.Failure.error("Invalid email verification code")
      case ApiError.InternalServerError          => UIError.Failure.internalDefect
      case ApiError.InvalidInput(error)          => UIError.Failure.error(error)
    }

  def toPageTask: PageTask[A] =
    self.toPageLoadTask.mapError {
      case fail: UIError.Failure => fail
      case UIError.Redirect(url) => UIError.Failure.internalDefect(s"Unable to redirect on non-page load: $url")
    }

}
