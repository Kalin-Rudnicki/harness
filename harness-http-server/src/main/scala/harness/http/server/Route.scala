package harness.http.server

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
  ): SHRIO[BuiltInRequestEnv & R, HttpResponse]

}
object Route {

  def oneOf[R](routes: Route[R]*): Route[R] = { (method, path) =>
    def rec(routes: List[Route[R]]): SHRIO[BuiltInRequestEnv & R, HttpResponse] =
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

  private val pageHtmlResponse: HttpResponse =
    HttpResponse(
      """<!DOCTYPE html>
        |<html lang="en">
        |
        |<head>
        |    <meta charset="UTF-8">
        |    <title>Title</title>
        |    <link rel='shortcut icon' type='image/x-icon' href='/res/favicon.ico' />
        |    <script id="scripts" src="/res/js/main.js"></script>
        |</head>
        |
        |<body>
        |</body>
        |
        |</html>""".stripMargin,
    )

  private def getFile(config: ServerConfig, after: List[String]): SHRIO[Scope, HttpResponse] = {
    val path = s"${config.resDir}${after.map(p => s"/$p").mkString}"

    if (config.useJarResource)
      HttpResponse.jarResourceOrNotFound(path)
    else
      Path(path).flatMap(HttpResponse.fileOrNotFound(_))
  }

  /**
    * {res} : resource dir, can be set via `--res-dir=VALUE`
    *
    * Expects things to be set up in the following format:
    *  - /api/{*}          ->  provided api routes
    *  - /api/health-check ->  runs health-check
    *  - /page/{*}         ->  {res}/index.html
    *  - /res/favicon.ico  ->  {res}/favicon.ico
    *  - /res/js/{*}       ->  {res}/js/{*}
    *  - /res/css/{*}      ->  {res}/css/{*}
    */
  def stdRoot[R](apis: Route[R]*): URIO[ServerConfig, Route[R]] =
    ZIO.serviceWith[ServerConfig] { config =>
      Route.oneOf(
        "api" /: Route.oneOf(apis*),
        (HttpMethod.GET / "api" / "health-check").implement { _ =>
          for {
            _ <- Logger.log.info("health-check")
            _ <- HttpRequest.query.logAll(Logger.LogLevel.Info)
            _ <- HttpRequest.header.logAll(Logger.LogLevel.Info)
            _ <- HttpRequest.cookie.logAll(Logger.LogLevel.Info)
          } yield HttpResponse.fromHttpCode.Ok
        },
        (HttpMethod.GET / "page" / RouteMatcher.**).implement { _ =>
          ZIO.succeed(pageHtmlResponse)
        },
        HttpMethod.GET /: "res" /: Route.oneOf(
          "favicon.ico".implement { _ =>
            getFile(config, "favicon.ico" :: Nil)
          },
          ("js" / RouteMatcher.**).implement { routes =>
            getFile(config, "js" :: routes)
          },
        ),
        HttpMethod.GET.implement { _ => ZIO.succeed(HttpResponse.redirect("/page")) },
      )
    }

}
