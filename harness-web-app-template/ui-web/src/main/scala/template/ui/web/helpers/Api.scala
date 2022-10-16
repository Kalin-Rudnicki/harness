package template.ui.web.helpers

import harness.web.client.*
import harness.zio.*
import template.model as D

object Api {

  object user {

    def fromSessionToken: HTask[D.user.User] =
      HttpRequest
        .get("/api/user/from-session-token")
        .noBody
        .jsonResponse[D.user.User]

    def fromSessionTokenOptional: HTask[Option[D.user.User]] =
      HttpRequest
        .get("/api/user/from-session-token-optional")
        .noBody
        .jsonResponse[Option[D.user.User]]

    def signUp(d: D.user.SignUp): HTask[Unit] =
      HttpRequest
        .post("/api/user/sign-up")
        .jsonBody(d)
        .unit200

    def login(d: D.user.Login): HTask[Unit] =
      HttpRequest
        .post("/api/user/login")
        .jsonBody(d)
        .unit200

    def logOut: HTask[Unit] =
      HttpRequest
        .post("/api/user/log-out")
        .noBody
        .unit200

  }

}
