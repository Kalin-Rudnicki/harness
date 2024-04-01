package harness.http.server

import cats.syntax.option.*
import com.sun.net.httpserver.{HttpExchange, HttpHandler}
import harness.endpoint.types.*
import harness.web.{HttpCode, HttpMethod}
import harness.web.Constants.harnessInternalErrorHeader
import harness.zio.*
import java.util.Base64
import scala.annotation.tailrec
import zio.*

final case class Handler[ServerEnv, ReqEnv: EnvironmentTag](
    serverRuntime: Runtime[HarnessEnv & ServerEnv],
    reqLayer: RLayer[ServerEnv & Scope, ReqEnv],
    endpoints: List[Endpoint[ServerEnv & ReqEnv, EndpointType.Any]],
    debugErrorHeader: Boolean,
) extends HttpHandler { self =>

  // TODO (KR) : --- Potential optimization ---
  //           : make endpoints a Ref, and have them be ordered by how often they are called
  //           : this is only worth it if it turns out finding the right endpoint is slower than desired

  private val groupedByMethod: Map[HttpMethod, List[Endpoint[ServerEnv & ReqEnv, EndpointType.Any]]] =
    endpoints.groupBy(_.spec.method)

  private def runImplAndHandleErrors[ET <: EndpointType.Any](
      request: HttpRequest,
      endpoint: Endpoint[ServerEnv & ReqEnv, ET],
  )(
      parsedPath: endpoint.spec.inputWithCookiesCodec.PathT,
  ): URIO[HarnessEnv & ServerEnv & Scope, HttpResponse[OutputResult]] = {
    val errorHandler = endpoint.implementation.errorHandler

    val convertError: Cause[endpoint.implementation.DomainError] => endpoint.implementation.DomainError = {
      case Cause.Fail(error, _) =>
        error
      case Cause.Die(error, _) =>
        errorHandler.convertUnexpectedError(error)
      case cause =>
        cause.unified.headOption.map(_.toThrowable) match {
          case Some(error) =>
            errorHandler.convertUnexpectedError(error)
          case None =>
            val error = new RuntimeException(s"Unable to get cause of error...\n$cause")
            errorHandler.convertUnexpectedError(error)
        }
    }

    endpoint
      .handleRaw(request, parsedPath)
      .flatMap { response => OutputResult.fromOutputStream(response.body).mapBoth(errorHandler.convertUnexpectedError, b => response.copy(body = b)) }
      .provideSomeLayer[HarnessEnv & ServerEnv & Scope] { ZLayer.succeed(request) ++ reqLayer.mapError(errorHandler.convertUnexpectedError) }
      .tapErrorCause(Logger.logErrorCauseSimple(_, Logger.LogLevel.Error, Logger.LogLevel.Debug.some)(using errorHandler.errorLogger))
      .foldCause(
        { cause =>
          val domainError = convertError(cause)
          val apiError = errorHandler.errorConverter.mapError(domainError)
          val (errorCode, errorBody) = endpoint.spec.errorCodec.encode(apiError)
          val optErrorHeader = Option.when(debugErrorHeader) {
            Base64.getEncoder.encodeToString(errorHandler.errorLogger.convert(domainError)._2.getBytes)
          }
          errorHandler
            .headersAndCookiesOnError(apiError)(HttpResponse(OutputResult.fromString(errorBody), errorCode))
            .withHeaders(harnessInternalErrorHeader, optErrorHeader.toList)
        },
        identity,
      )
  }

  @tailrec
  private def loop(
      startMarker: Telemetry.StartMarker,
      request: HttpRequest,
      endpoints: List[Endpoint[ServerEnv & ReqEnv, EndpointType.Any]], // expected to only have the right method
  ): URIO[HarnessEnv & ServerEnv & Scope, HttpResponse[OutputResult]] =
    endpoints match {
      case eHead :: eTail =>
        eHead.spec.inputWithCookiesCodec.decodePath(request.path) match {
          case Some(parsedPath) =>
            for {
              _ <- startMarker.markEnd("HTTP Request Path Parsed", Logger.LogLevel.Debug, true, "method" -> request.method.method, "path" -> request.pathString)
              result <- runImplAndHandleErrors(request, eHead)(parsedPath)
            } yield result
          case None =>
            loop(startMarker, request, eTail)
        }
      case Nil =>
        (self.endpoints.flatMap(e => e.spec.inputWithCookiesCodec.decodePath(request.path).map(_ => e.spec.method)) match {
          case Nil =>
            ZIO.succeed(HttpResponse(OutputResult.fromString("Not Found"), HttpCode.`404`))
          case methods =>
            val message = s"Method ${request.method.method} not allowed. Allowed: ${methods.map(_.method).distinct.sorted.mkString(", ")}"
            ZIO.succeed(HttpResponse(OutputResult.fromString(message), HttpCode.`405`))
        }).tap { response =>
          startMarker.markEnd("HTTP Request Path Not Found", Logger.LogLevel.Debug, true, "method" -> request.method.method, "path" -> request.pathString, "code" -> response.code.code) *>
            Logger.log.info("Server received request with incorrect method/path", "method" -> request.method.method, "path" -> request.pathString, "code" -> response.code.code)
        }
    }

  private def handleZIO(exchange: HttpExchange): URIO[HarnessEnv & ServerEnv, Unit] =
    Random.nextUUID.flatMap { requestId =>
      Logger.addContext("request-id" -> requestId) {
        ZIO
          .scoped {
            for {
              responseBody <- ZIO.fromAutoCloseable { ZIO.succeed(exchange.getResponseBody) }

              request <- ZIO.attempt { HttpRequest.read(exchange, requestId) }
              startMarker <- Telemetry.StartMarker.make
              response <- loop(startMarker, request, groupedByMethod.getOrElse(request.method, Nil))

              _ <- ZIO.attempt {
                val headers = exchange.getResponseHeaders
                response.headers.foreach { (k, vs) => vs.foreach(headers.add(k, _)) }
                response.cookies.reverse.foreach { c => headers.add("Set-Cookie", c.cookieString) }
              }
              _ <- ZIO.attempt { exchange.sendResponseHeaders(response.code.code, response.body.length) }
              _ <- ZIO.attempt { response.body.writeOutput(responseBody) }
            } yield ()
          }
          .logErrorCauseSimpleAndContinue(Logger.LogLevel.Error, Logger.LogLevel.Debug.some)(using ErrorLogger.ThrowableInstances.getMessageErrorLogger)
          .unit
          .telemetrize("HTTP Request Handler", Logger.LogLevel.Detailed, "method" -> exchange.getRequestMethod, "path" -> exchange.getRequestURI.getPath)
      }

    }

  override def handle(httpExchange: HttpExchange): Unit =
    Unsafe.unsafely { serverRuntime.unsafe.run(handleZIO(httpExchange)) }

}
