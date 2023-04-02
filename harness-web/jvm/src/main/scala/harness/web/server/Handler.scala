package harness.web.server

import cats.data.NonEmptyList
import com.sun.net.httpserver.*
import harness.core.*
import harness.web.*
import harness.zio.*
import java.util.UUID
import zio.*
import zio.json.*

final case class Handler[ServerEnv, ReqEnv: EnvironmentTag](
    serverRuntime: Runtime[HarnessEnv & ServerEnv],
    reqLayer: HRLayer[ServerEnv & Scope, ReqEnv],
    route: Route[ServerEnv & ReqEnv],
) extends HttpHandler {

  override def handle(exchange: HttpExchange): Unit = {
    val requestId: UUID = UUID.randomUUID

    val builtInReqLayer: HTaskLayer[BuiltInRequestEnv] =
      ZLayer.fromZIO(ZIO.hAttempt(HttpRequest.read(exchange, requestId))) ++
        Scope.default

    val effect: SHRIO[ServerEnv & BuiltInRequestEnv & ReqEnv, Unit] =
      ZIO.serviceWithZIO[HttpRequest] { req =>
        (for {
          now <- Clock.currentDateTime
          responseBody <- ZIO.fromAutoCloseable { ZIO.succeed(exchange.getResponseBody) }
          ifHidden <- ZIO.service[HError.UserMessage.IfHidden]

          _ <- Logger.log.info(s"received ${req.method.method} request @ '${req.pathString}'", "date" -> now.toLocalDate, "time" -> now.toOffsetTime)

          responseOrError <- route(req.method, req.path).either
          foundResponse <-
            responseOrError match {
              case Right(found: HttpResponse.Found) => ZIO.succeed(found)
              case Right(HttpResponse.NotFound)     => ZIO.succeed(HttpResponse("Not Found", HttpCode.`404`))
              case Left(error) =>
                val (specifiedResponseCode, errors) = Handler.getHttpCodeAndErrors(error)
                val responseCode =
                  specifiedResponseCode match {
                    case Some(responseCode) => responseCode
                    case None =>
                      if (errors.forall(_.isInstanceOf[HError.UserError])) HttpCode.`400`
                      else HttpCode.`500`
                  }
                val resp = HttpResponse(errors.toList.map(_.userMessage.show(ifHidden)).toJson, responseCode)
                val logErrors =
                  ZIO.foreachDiscard(errors.toList) { e =>
                    Logger.log.error(e.fullInternalMessage) *>
                      Logger.log.debug(e.fullInternalMessageWithTrace)
                  }

                logErrors.as(resp)
            }

          _ <-
            ZIO
              .hAttempt {
                val headers = exchange.getResponseHeaders
                foundResponse.headers.foreach { (k, v) => headers.set(k, v) }
                foundResponse.cookies.reverse.foreach { c => headers.add("Set-Cookie", c.cookieString) }
              }
              .mapError(HError.SystemFailure("Unable to write response cookies", _))
          _ <- ZIO.hAttempt(exchange.sendResponseHeaders(foundResponse.code.code, foundResponse.length)).mapError(HError.SystemFailure("Unable to write response headers", _))
          _ <- ZIO.hAttempt(foundResponse.write(responseBody)).mapError(HError.SystemFailure("Unable to write response body", _))
        } yield ()).trace("HTTP Request Handler", Logger.LogLevel.Detailed, "path" -> req.pathString)
      }

    Unsafe.unsafe { implicit unsafe =>
      serverRuntime.unsafe.run {
        ZIO.scoped {
          Logger
            .addContext("request-id" -> requestId) {
              effect
                .provideSomeLayer[HarnessEnv & ServerEnv & Scope](builtInReqLayer ++ reqLayer)
                .trace("Full HTTP Request Handler")
            }
            .dumpErrorsAndContinue
        }
      }
    }
  }

}
object Handler {

  private def getHttpCodeAndErrors(error: HError): (Option[HttpCode], NonEmptyList[HError.Single]) = {
    def rec(error: HError): (List[HttpCode], NonEmptyList[HError.Single]) =
      error match {
        case ErrorWithHTTPCode(httpCode, child) =>
          val (childCodes, childErrors) = rec(child)
          (httpCode :: childCodes, childErrors)
        case error: HError.Single => (Nil, NonEmptyList.one(error))
        case HError.Multiple(children) =>
          val mapped = children.map(rec)
          (mapped.toList.flatMap(_._1), mapped.flatMap(_._2))
      }

    val (codes, errors) = rec(error)
    (codes.distinct.maxByOption(_.code), errors)
  }

}
