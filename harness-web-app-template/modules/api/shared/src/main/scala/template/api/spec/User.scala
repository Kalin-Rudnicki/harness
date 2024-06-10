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
    resendEmailVerification: F[User.ResendEmailVerification],
)
object User {

  type Get = EndpointType.Builder#Auth[A.user.UserToken]#OutputBodyEncoded[A.user.User]#Error[ApiError]#Build
  type Login = EndpointType.Builder#InputBodyEncoded[A.user.Login]#OutputBodyEncoded[A.user.User]#Error[ApiError]#Build
  type LogOut = EndpointType.Builder#Auth[A.user.UserToken]#Error[ApiError]#Build
  type SignUp = EndpointType.Builder#InputBodyEncoded[A.user.SignUp]#OutputBodyEncoded[A.user.User]#Error[ApiError]#Build
  type VerifyEmail = EndpointType.Builder#Query[A.user.EmailVerificationCode]#Auth[A.user.UserToken]#Error[ApiError]#Build
  type ResendEmailVerification = EndpointType.Builder#Auth[A.user.UserToken]#Error[ApiError]#Build

  def spec(authToken: headerOrCookie[A.user.UserToken]): User[EndpointSpec] =
    "user" /:
      User[EndpointSpec](
        get = EndpointSpec.get("Get User Info")
          /!# authToken /--> body.json[A.user.User] /!--> errorBody.json[ApiError],
        login = EndpointSpec.post("Login") / "login"
          /<-- body.json[A.user.Login] /--> body.json[A.user.User] /!--> errorBody.json[ApiError],
        logOut = EndpointSpec.post("Log Out") / "log-out"
          /!# authToken /!--> errorBody.json[ApiError],
        signUp = EndpointSpec.post("Sign Up") / "sign-up"
          /<-- body.json[A.user.SignUp] /--> body.json[A.user.User] /!--> errorBody.json[ApiError],
        verifyEmail = EndpointSpec.post("Verify Email") / "email" / "verify"
          /? query[A.user.EmailVerificationCode]("code") /!# authToken /!--> errorBody.json[ApiError],
        resendEmailVerification = EndpointSpec.post("Resend Email Verification") / "email" / "resend-verification"
          /!# authToken /!--> errorBody.json[ApiError],
      )

}
