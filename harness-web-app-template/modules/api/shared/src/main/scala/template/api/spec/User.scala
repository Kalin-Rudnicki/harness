package template.api.spec

import harness.endpoint.spec.*
import harness.endpoint.typeclass.*
import harness.endpoint.types.*
import template.api.model.error.ApiError
import template.api.model as A

final case class User[F[_ <: EndpointType.Any]](
    get: F[User.Get],
    login: F[User.Login],
    logOut: F[User.LogOut],
    signUp: F[User.SignUp],
    verifyEmail: F[User.VerifyEmail],
    resendEmailCode: F[User.ResendEmailCode],
)
object User {

  type Get = EndpointType[A.user.UserToken, Unit, BodyType.None, BodyType.Encoded[A.user.User], ApiError]
  type Login = EndpointType.Basic[Unit, BodyType.Encoded[A.user.Login], BodyType.Encoded[A.user.User], ApiError]
  type LogOut = EndpointType[A.user.UserToken, Unit, BodyType.None, BodyType.None, ApiError]
  type SignUp = EndpointType.Basic[Unit, BodyType.Encoded[A.user.SignUp], BodyType.Encoded[A.user.User], ApiError]
  type VerifyEmail = EndpointType[(A.user.EmailVerificationCode, A.user.UserToken), A.user.EmailVerificationCode, BodyType.None, BodyType.None, ApiError]
  type ResendEmailCode = EndpointType[A.user.UserToken, Unit, BodyType.None, BodyType.None, ApiError]

  def spec(authToken: headerOrCookie[A.user.UserToken]): User[EndpointSpec] =
    "user" /:
      User[EndpointSpec](
        get = EndpointSpec.get("Get User Info")
          /# authToken /--> body.json[A.user.User] /!--> errorBody.json[ApiError],
        login = EndpointSpec.post("Login") / "login"
          /<-- body.json[A.user.Login] /--> body.json[A.user.User] /!--> errorBody.json[ApiError],
        logOut = EndpointSpec.post("Log Out") / "log-out"
          /# authToken /!--> errorBody.json[ApiError],
        signUp = EndpointSpec.post("Sign Up") / "sign-up"
          /<-- body.json[A.user.SignUp] /--> body.json[A.user.User] /!--> errorBody.json[ApiError],
        verifyEmail = EndpointSpec.post("Verify Email") / "email" / "verify"
          /? query[A.user.EmailVerificationCode]("code") /# authToken /!--> errorBody.json[ApiError],
        resendEmailCode = EndpointSpec.post("Resend Email Verification") / "email" / "resend-verification"
          /# authToken /!--> errorBody.json[ApiError],
      )

}
