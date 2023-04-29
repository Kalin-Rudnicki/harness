package harness.archive.api.user

import harness.archive.api.*
import harness.archive.api.generators as G
import harness.archive.model as D
import harness.core.HError
import harness.http.server.*
import harness.zio.Logger
import zio.test.*
import zio.test.Assertion.*

object UserSpec extends CustomRouteSpec {

  override lazy val logLevel: Logger.LogLevel = Logger.LogLevel.Debug

  override def routeSpec: TestSpec =
    suite("UserSpec")(
      test("can sign up") {
        check(G.User.signUpGen) { signUp =>
          for {
            _ <- httpRequest(HttpRequest.builder.post("api", "user", "sign-up").jsonBody(signUp))
            user <- httpRequest.jsonResponse[D.user.User](HttpRequest.builder.get("api", "user", "from-session-token").noBody)
          } yield assert(user.username)(equalTo(signUp.username))
        }
      },
      test("can sign out") {
        check(G.User.signUpGen) { signUp =>
          for {
            _ <- httpRequest(HttpRequest.builder.post("api", "user", "sign-up").jsonBody(signUp))
            _ <- httpRequest(HttpRequest.builder.post("api", "user", "log-out").noBody)
            res <- httpRequest(HttpRequest.builder.get("api", "user", "from-session-token").noBody).either
          } yield assert(res)(isLeft(isSubtype[HError.UserError](anything)))
        }
      },
      test("can log in") {
        check(G.User.signUpGen) { signUp =>
          for {
            _ <- httpRequest(HttpRequest.builder.post("api", "user", "sign-up").jsonBody(signUp))
            _ <- httpRequest(HttpRequest.builder.post("api", "user", "log-out").noBody)
            _ <- httpRequest(HttpRequest.builder.post("api", "user", "login").jsonBody(D.user.Login(signUp.username, signUp.password)))
            user <- httpRequest.jsonResponse[D.user.User](HttpRequest.builder.get("api", "user", "from-session-token").noBody)
          } yield assert(user.username)(equalTo(signUp.username))
        }
      },
    )

}
