package harness.http.server

import cats.syntax.option.*
import harness.web.*
import harness.zio.*
import scala.annotation.tailrec
import zio.*
import zio.json.*
import zio.json.ast.Json

trait Route[-R] { self =>

  final def /:(met: HttpMethod): Route[R] =
    (method, path) =>
      if (method == met) self(method, path)
      else Route.Result.NotFound

  final def /:(str: String): Route[R] =
    (method, path) =>
      path match {
        case head :: tail if head == str => self(method, tail)
        case _                           => Route.Result.NotFound
      }

  final def widen[R2 <: R]: Route[R2] = self

  def apply(
      method: HttpMethod,
      path: List[String],
  ): Route.Result[R]

}
object Route {

  sealed trait Result[-R]
  object Result {

    case object NotFound extends Result[Any]

    final case class Found[-R, DomainError, ApiError](
        effect: ZIO[HarnessEnv & BuiltInRequestEnv & R, DomainError, HttpResponse],
        handler: ErrorHandler[DomainError, ApiError],
    ) extends Result[R]

  }

  def oneOf[R](routes: Route[R]*): Route[R] = { (method, path) =>
    @tailrec
    def loop(routes: List[Route[R]]): Result[R] =
      routes match {
        case head :: tail =>
          head(method, path) match {
            case found @ Result.Found(_, _) => found
            case Result.NotFound            => loop(tail)
          }
        case Nil => Result.NotFound
      }

    loop(routes.toList)
  }

  private def pageHtmlResponse(harnessUiConfig: HConfig): HttpResponse =
    HttpResponse(
      s"""<!DOCTYPE html>
        |<html lang="en">
        |
        |<head>
        |    <meta charset="UTF-8">
        |    <title>Title</title>
        |    <link rel='shortcut icon' type='image/x-icon' href='/res/favicon.ico' />
        |    <script>
        |      const harnessUiConfig = ${Json.Str(harnessUiConfig.configJson.toJson).toJson}
        |    </script>
        |    <script id="scripts" src="/res/js/main.js"></script>
        |</head>
        |
        |<body>
        |</body>
        |
        |</html>""".stripMargin,
    )

  private def getFile(config: ServerConfig, after: List[String]): RIO[HarnessEnv & Scope, HttpResponse] = {
    val path = s"${config.resDir}${after.map(p => s"/$p").mkString}"

    if (config.useJarResource)
      HttpResponse.jarResource(path)
    else
      Path(path).flatMap(HttpResponse.file(_))
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
  def stdRoot[A <: HasStdClientConfig: JsonEncoder, R](harnessUiConfig: A)(apis: Route[R]*): URIO[ServerConfig, Route[R]] =
    ZIO.serviceWith[ServerConfig] { config =>
      val pageHtml = pageHtmlResponse(HConfig.unsafeFromEncodable(harnessUiConfig))

      Route.oneOf(
        "api" /: Route.oneOf(apis*),
        (HttpMethod.GET / "api" / "health-check").implementGenericError { _ =>
          for {
            _ <- Logger.log.info("health-check")
            _ <- HttpRequest.query.logAll(Logger.LogLevel.Info)
            _ <- HttpRequest.header.logAll(Logger.LogLevel.Info)
            _ <- HttpRequest.cookie.logAll(Logger.LogLevel.Info)
          } yield HttpResponse.fromHttpCode.Ok
        },
        (HttpMethod.GET / "page" / RouteMatcher.**).implementGenericError { _ =>
          ZIO.succeed(pageHtml)
        },
        HttpMethod.GET /: "res" /: Route.oneOf(
          "favicon.ico".implementGenericError { _ =>
            getFile(config, "favicon.ico" :: Nil)
          },
          ("js" / RouteMatcher.**).implementGenericError { routes =>
            getFile(config, "js" :: routes)
          },
        ),
        HttpMethod.GET.implementGenericError { _ => ZIO.succeed(HttpResponse.redirect("/page")) },
      )
    }

}
