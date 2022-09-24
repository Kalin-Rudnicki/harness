package template.ui.web.helpers

import harness.web.client.*
import harness.zio.*
import template.model as D

object Api {

  object user {

    def fromSessionToken: HTaskN[D.user.User] =
      HttpRequest
        .get("/api/user/from-session-token")
        .noBody
        .jsonResponse[D.user.User]

    def fromSessionTokenOptional: HTaskN[Option[D.user.User]] =
      HttpRequest
        .get("/api/user/from-session-token-optional")
        .noBody
        .jsonResponse[Option[D.user.User]]

    def signUp(d: D.user.SignUp): HTaskN[Unit] =
      HttpRequest
        .post("/api/user/sign-up")
        .jsonBody(d)
        .unit200

    def login(d: D.user.Login): HTaskN[Unit] =
      HttpRequest
        .post("/api/user/login")
        .jsonBody(d)
        .unit200

    def logOut: HTaskN[Unit] =
      HttpRequest
        .post("/api/user/log-out")
        .noBody
        .unit200

  }

}
