package harness.http.server

import cats.syntax.option.*
import com.sun.net.httpserver.*
import harness.core.*
import harness.http.server.Route.Result
import harness.web.*
import harness.web.Constants.harnessInternalErrorHeader
import harness.zio.*
import java.util.Base64
import java.util.UUID
import zio.*

final case class Handler[ServerEnv, ReqEnv: EnvironmentTag](
    serverRuntime: Runtime[HarnessEnv & ServerEnv],
    reqLayer: RLayer[ServerEnv & Scope, ReqEnv],
    route: Route[ServerEnv & ReqEnv],
    debugErrorHeader: Boolean,
) extends HttpHandler {

  override def handle(exchange: HttpExchange): Unit = {
    val requestId: UUID = UUID.randomUUID

    val builtInReqLayer: TaskLayer[BuiltInRequestEnv] =
      ZLayer.fromZIO(ZIO.attempt(HttpRequest.read(exchange, requestId))) ++
        Scope.default

    def convertError[DomainError](cause: Cause[DomainError], handler: ErrorHandler[DomainError, ?]): DomainError = {
      def rec(cause: Cause[DomainError]): (Int, DomainError) =
        cause match {
          case Cause.Empty                 => 0 -> handler.convertUnexpectedError(new RuntimeException("Failed with Empty cause"))
          case Cause.Fail(value, _)        => 3 -> value
          case Cause.Die(value, _)         => 2 -> handler.convertUnexpectedError(value)
          case Cause.Interrupt(fiberId, _) => 1 -> handler.convertUnexpectedError(new RuntimeException(s"Interrupted by fiber $fiberId"))
          case Cause.Stackless(cause, _)   => rec(cause)
          case Cause.Then(left, right) =>
            val convertedLeft = rec(left)
            val convertedRight = rec(right)
            if (convertedLeft._1 >= convertedRight._1) convertedLeft
            else convertedRight
          case Cause.Both(left, right) =>
            val convertedLeft = rec(left)
            val convertedRight = rec(right)
            if (convertedLeft._1 >= convertedRight._1) convertedLeft
            else convertedRight
        }

      rec(cause)._2
    }

    def runFound[DomainError, ApiError](result: Route.Result.Found[ServerEnv & ReqEnv, DomainError, ApiError]): URIO[BuiltInRequestEnv & HarnessEnv & ServerEnv, HttpResponse.Found] =
      result.effect
        .provideSomeLayer[BuiltInRequestEnv & HarnessEnv & ServerEnv](reqLayer.mapError(result.handler.convertUnexpectedError(_)))
        .foldCauseZIO(
          cause =>
            Logger.logErrorCauseSimple[DomainError](cause, Logger.LogLevel.Error, Logger.LogLevel.Debug.some)(using result.handler.errorLogger.withPrefix("Error in http server:\n")).as {
              val domainError = convertError(cause, result.handler)
              val apiError = result.handler.errorConverter.mapError(domainError)
              val apiErrorString = result.handler.errorCodec.encode(apiError)
              val errorCode = result.handler.errorCode(apiError)
              val optErrorHeader = Option.when(debugErrorHeader) {
                Base64.getEncoder.encodeToString(result.handler.errorLogger.convert(domainError)._2.getBytes)
              }
              HttpResponse(apiErrorString, errorCode).optHeader(harnessInternalErrorHeader, optErrorHeader)
            },
          {
            case found: HttpResponse.Found => ZIO.succeed(found)
            case HttpResponse.NotFound     => ZIO.succeed(HttpResponse.fromHttpCode.`404`)
          },
        )

    val effect: RIO[BuiltInRequestEnv & HarnessEnv & ServerEnv, Unit] =
      ZIO.scoped {
        for {
          req <- HttpRequest.service
          now <- Clock.currentDateTime
          responseBody <- ZIO.fromAutoCloseable { ZIO.succeed(exchange.getResponseBody) }

          _ <- Logger.log.info(s"received ${req.method.method} request @ '${req.pathString}'", "remote-address" -> req.remoteAddress, "date" -> now.toLocalDate, "time" -> now.toOffsetTime)

          foundResponse: HttpResponse.Found <- route(req.method, req.path) match {
            case found @ Result.Found(_, _) => runFound(found)
            case Result.NotFound =>
              Logger.log.warning("Path/Method not supported", "path" -> req.pathString, "method" -> req.method) *>
                ZIO.succeed(HttpResponse.fromHttpCode.`404`)
          }

          _ <-
            ZIO
              .attempt {
                val headers = exchange.getResponseHeaders
                foundResponse.headers.foreach { (k, v) => headers.set(k, v) }
                foundResponse.cookies.reverse.foreach { c => headers.add("Set-Cookie", c.cookieString) }
              }
              .mapError(new RuntimeException("Unable to write response cookies", _))
          _ <- HttpResponse.Return.`return`(
            foundResponse.`return`,
            responseBody,
            bodyLength => ZIO.attempt(exchange.sendResponseHeaders(foundResponse.code.code, bodyLength)).mapError(new RuntimeException("Unable to write response headers", _)),
          )
        } yield ()
      }

    Unsafe.unsafe { implicit unsafe =>
      serverRuntime.unsafe.run {
        Logger
          .addContext("request-id" -> requestId) {
            effect
              .provideSomeLayer[HarnessEnv & ServerEnv](builtInReqLayer)
              .telemetrize("HTTP Request Handler", Logger.LogLevel.Detailed, "path" -> exchange.getRequestURI.getPath)
          }
          .logErrorCauseSimpleAndContinue(Logger.LogLevel.Error, Logger.LogLevel.Debug.some)(using ErrorLogger.throwablePrettyErrorLogger)
      }
    }
  }

}
