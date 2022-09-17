package harness.web.server

import harness.core.*
import harness.zio.*
import zio.*

trait Route[-R] { self =>

  final def /:(str: String): Route[R] =
    (method, path) =>
      path match {
        case head :: tail if head == str => self(method, tail)
        case _                           => ZIO.succeed(HttpResponse.NotFound)
      }

  final def widen[R2 <: R]: Route[R2] = self

  def apply(
      method: HttpMethod,
      path: List[String],
  ): SHRION[ServerEnv & RequestEnv & R, HttpResponse]

}
object Route {

  def oneOf[R](routes: Route[R]*): Route[R] = { (method, path) =>
    def rec(routes: List[Route[R]]): SHRION[ServerEnv & RequestEnv & R, HttpResponse] =
      routes match {
        case head :: tail =>
          head(method, path).flatMap {
            case HttpResponse.NotFound => rec(tail)
            case response              => ZIO.succeed(response)
          }
        case Nil => ZIO.succeed(HttpResponse.NotFound)
      }

    rec(routes.toList)
  }

  def root[R](apis: Route[R]*): Route[R] =
    Route.oneOf(
      "api" /: Route.oneOf(apis*),
      // TODO (KR) :
      (HttpMethod.GET / "page" / RouteMatcher.**).implement { _ => ZIO.failNel(HError.???("return page")) },
      // TODO (KR) :
      (HttpMethod.GET / "favicon.ico" / RouteMatcher.**).implement { _ => ZIO.failNel(HError.???("return favicon.ico")) },
      HttpMethod.GET.implement { _ => ZIO.succeed(HttpResponse.redirect("/page")) },
    )

}
