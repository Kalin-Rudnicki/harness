package template.api.user

import harness.core.HError
import harness.http.server.*
import harness.zio.Logger
import template.api.*
import template.api.generators as G
import template.model as D
import zio.test.*
import zio.test.Assertion.*

object UserSpec extends CustomRouteSpec {

  override lazy val logLevel: Logger.LogLevel = Logger.LogLevel.Debug

  override def routeSpec: TestSpec =
    test("can sign up") {
      check(G.User.signUpGen) { signUp =>
        for {
          _ <- httpRequest(HttpRequest.builder.post("api", "user", "sign-up").jsonBody(signUp))
          user <- httpRequest.jsonResponse[D.user.User](HttpRequest.builder.get("api", "user", "from-session-token").noBody)
        } yield assert(user.username)(equalTo(signUp.username))
      }
    }

}
