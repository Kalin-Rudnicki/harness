package harness.http.server

import harness.core.*
import harness.endpoint.StandardPattern
import harness.endpoint.transfer.*
import harness.endpoint.types.EndpointType
import harness.web.HasStdClientConfig
import harness.zio.*
import harness.zio.error.{FSError, JarResourceError}
import zio.*
import zio.json.*
import zio.json.ast.Json

implicit class StandardPatternCompanionOps(self: StandardPattern.type) {

  def impl[T[_[_ <: EndpointType.Any]], R, Cfg <: HasStdClientConfig: JsonEncoder](
      serverConfig: Server.Config,
      clientConfig: Cfg,
  )(
      tImpl: T[Implementation.Projection[R]],
  ): StandardPattern[T, Implementation.Projection[R]] = {

    sealed trait DomainError {
      override final def toString: String = this match {
        case DomainError.NotFound(message)     => s"Not found: $message"
        case DomainError.InternalDefect(cause) => s"Internal defect: ${cause.safeGetMessage}"
      }
    }
    object DomainError {
      final case class NotFound(message: String) extends DomainError
      final case class InternalDefect(cause: Throwable) extends DomainError
    }

    implicit val apiErrorHandler: ErrorHandler.Id[DomainError, StandardPattern.ApiError] =
      ErrorHandler.id[DomainError, StandardPattern.ApiError](
        DomainError.InternalDefect(_),
        DomainError.InternalDefect(_),
        {
          case DomainError.NotFound(_)       => StandardPattern.ApiError.NotFound
          case DomainError.InternalDefect(_) => StandardPattern.ApiError.InternalDefect
        },
        ErrorLogger.withToString[DomainError].atLevel.error,
        _ => identity,
      )

    val pageHtmlResponse: HttpResponse[String] =
      HttpResponse(
        s"""<!DOCTYPE html>
           |<html lang="en">
           |
           |<head>
           |    <meta charset="UTF-8">
           |    <title>Title</title>
           |    <link rel='shortcut icon' type='image/x-icon' href='/res/favicon.ico' />
           |    <script>
           |      const harnessUiConfig = ${Json.Str(clientConfig.toJson).toJson}
           |    </script>
           |    <script id="scripts" src="/res/js/main.js"></script>
           |</head>
           |
           |<body>
           |</body>
           |
           |</html>""".stripMargin,
      )

    def getResFile(path: List[String]): ZIO[FileSystem & Scope, DomainError, OutputStream] = {
      val reprPath = ("res" :: path).mkString("/", "/", "")
      val actualPath = (serverConfig.resDir :: path).mkString("/")

      if (serverConfig.useJarResource)
        JarUtils
          .getInputStream(actualPath)
          .refineOrDie { case _: JarResourceError.PathDNE => DomainError.NotFound(reprPath) }
          .map { OutputStream.ForwardRaw(_) }
      else
        for {
          resDir <- Path(serverConfig.resDir).orDie
          file <- resDir.child(path.mkString("/")).orDie
          _ <- file.ensureIsFile.refineOrDie {
            case _: FSError.PathDNE        => DomainError.NotFound(reprPath)
            case _: FSError.PathIsNotAFile => DomainError.NotFound(reprPath)
          }
        } yield OutputStream.File(file)
    }

    StandardPattern[T, Implementation.Projection[R]](
      api = tImpl,
      healthCheck = Implementation[StandardPattern.HealthCheck].implement { _ =>
        ZIO.unit.toHttpResponse
      },
      index = Implementation[StandardPattern.Index].implement { _ =>
        ZIO.succeed { HttpResponse.redirect("/page") }
      },
      page = Implementation[StandardPattern.Page].implement { _ =>
        ZIO.succeed(pageHtmlResponse)
      },
      favicon = Implementation[StandardPattern.Favicon].implement { _ =>
        getResFile("favicon.ico" :: Nil).toHttpResponse
      },
      js = Implementation[StandardPattern.Js].implement { paths =>
        getResFile("js" :: paths).toHttpResponse
      },
    )
  }

}

implicit class ZIOHttpResponseOps[R, E, A](self: ZIO[R, E, A]) {
  def toHttpResponse: ZIO[R, E, HttpResponse[A]] = self.map(HttpResponse(_))
}
