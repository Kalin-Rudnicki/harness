package harness.archive.ui.web.helpers

import harness.archive.model as D
import harness.http.client.{HttpClient, HttpRequest}
import harness.webUI.*
import harness.zio.*
import zio.*

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

    def fromSessionTokenOrRedirectToLogin: HRIO[HttpClient.ClientT & Logger & Telemetry, D.user.User] =
      fromSessionTokenOptional.flatMap {
        case Some(user) => ZIO.succeed(user)
        case None       => ZIO.fail(Page.PageLoadRedirect(Url("page", "login")()))
      }

    def redirectToHomeIfLoggedIn: HRIO[HttpClient.ClientT & Logger & Telemetry, Unit] =
      fromSessionTokenOptional.flatMap {
        case Some(_) => ZIO.fail(Page.PageLoadRedirect(Url("page", "home")()))
        case None    => ZIO.unit
      }

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

  object app {

    def getAll: HRIO[HttpClient.ClientT & Logger & Telemetry, Chunk[D.app.App]] =
      HttpRequest
        .get("/api/app/get-all")
        .withNoBody
        .response
        .jsonBody[Chunk[D.app.App]]

  }

  object log {

    def getForApp(appName: String): HRIO[HttpClient.ClientT & Logger & Telemetry, Chunk[D.log.Log]] =
      HttpRequest
        .get("/api/log/get-for-app")
        .withQueryParam("app-name", appName)
        .withNoBody
        .response
        .jsonBody[Chunk[D.log.Log]]

    def get(query: String): HRIO[HttpClient.ClientT & Logger & Telemetry, Chunk[D.log.Log]] =
      Logger.log.info(s"query:\n$query") *>
        HttpRequest
          .get("/api/log/get")
          .withQueryParam("query", query)
          .withNoBody
          .response
          .jsonBody[Chunk[D.log.Log]]

  }

}
