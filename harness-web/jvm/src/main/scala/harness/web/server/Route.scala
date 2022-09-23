package harness.web.server

import harness.core.*
import harness.web.*
import harness.zio.*
import zio.*

trait Route[-R] { self =>

  final def /:(met: HttpMethod): Route[R] =
    (method, path) =>
      if (method == met) self(method, path)
      else ZIO.succeed(HttpResponse.NotFound)

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

  /**
    * {res} : resource dir, can be set via `--res-dir=VALUE`
    *
    * Expects things to be set up in the following format:
    *  - /api/{*}          ->  provided api routes
    *  - /page/{*}         ->  {res}/index.html
    *  - /res/favicon.ico  ->  {res}/favicon.ico
    *  - /res/js/{*}       ->  {res}/js/{*}
    *  - /res/css/{*}      ->  {res}/css/{*}
    */
  def stdRoot[R](config: ServerConfig)(apis: Route[R]*): Route[R] =
    Route.oneOf(
      "api" /: Route.oneOf(apis*),
      (HttpMethod.GET / "page" / RouteMatcher.**).implement { _ =>
        (for {
          resDir <- Path(config.resDir)
          file <- resDir.child("index.html")
          response <- HttpResponse.fileOrNotFound(file)
        } yield response).toErrorNel
      },
      HttpMethod.GET /: "res" /: Route.oneOf(
        "favicon.ico".implement { _ =>
          (for {
            resDir <- Path(config.resDir)
            file <- resDir.child("favicon.ico")
            response <- HttpResponse.fileOrNotFound(file)
          } yield response).toErrorNel
        },
        ("js" / RouteMatcher.**).implement { routes =>
          (for {
            resDir <- Path(config.resDir)
            file <- resDir.child(("js" :: routes).mkString("/"))
            response <- HttpResponse.fileOrFail(file)
          } yield response).toErrorNel
        },
        ("css" / RouteMatcher.**).implement { routes =>
          (for {
            resDir <- Path(config.resDir)
            file <- resDir.child(("css" :: routes).mkString("/"))
            response <- HttpResponse.fileOrFail(file)
          } yield response).toErrorNel
        },
      ),
      HttpMethod.GET.implement { _ => ZIO.succeed(HttpResponse.redirect("/page")) },
    )

}
