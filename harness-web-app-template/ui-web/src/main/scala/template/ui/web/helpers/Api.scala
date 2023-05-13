package template.ui.web.helpers

import harness.http.client.{HttpClient, HttpRequest}
import harness.webUI.*
import harness.zio.*
import template.model as D

object Api {

  object user {

    def fromSessionToken: HRIO[HttpClient.ClientT & Logger & Telemetry, D.user.User] =
      HttpRequest
        .get("/api/user/from-session-token")
        .withNoBody
        .response
        .jsonBody[D.user.User]

    def fromSessionTokenOptional: HRIO[HttpClient.ClientT & Logger & Telemetry, Option[D.user.User]] =
      HttpRequest
        .get("/api/user/from-session-token-optional")
        .withNoBody
        .response
        .jsonBody[Option[D.user.User]]

    def signUp(d: D.user.SignUp): HRIO[HttpClient.ClientT & Logger & Telemetry, Unit] =
      HttpRequest
        .post("/api/user/sign-up")
        .withBodyJsonEncoded(d)
        .response
        .unit2xx

    def login(d: D.user.Login): HRIO[HttpClient.ClientT & Logger & Telemetry, Unit] =
      HttpRequest
        .post("/api/user/login")
        .withBodyJsonEncoded(d)
        .response
        .unit2xx

    def logOut: HRIO[HttpClient.ClientT & Logger & Telemetry, Unit] =
      HttpRequest
        .post("/api/user/log-out")
        .withNoBody
        .response
        .unit2xx

  }

}
