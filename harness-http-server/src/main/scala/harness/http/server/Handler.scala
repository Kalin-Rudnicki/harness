package harness.http.server

import cats.data.NonEmptyList
import cats.syntax.either.*
import cats.syntax.option.*
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
      for {
        req <- HttpRequest.service
        now <- Clock.currentDateTime
        responseBody <- ZIO.fromAutoCloseable { ZIO.succeed(exchange.getResponseBody) }
        ifHidden <- ZIO.service[HError.UserMessage.IfHidden]

        _ <- Logger.log.info(s"received ${req.method.method} request @ '${req.pathString}'", "remote-address" -> req.remoteAddress, "date" -> now.toLocalDate, "time" -> now.toOffsetTime)

        responseOrError <- route(req.method, req.path).either
        responseOrError <- responseOrError match {
          case Left(earlyReturn: EarlyReturn) => Logger.log.debug("Received early return value").as(earlyReturn.response.asRight)
          case other                          => ZIO.succeed(other)
        }
        foundResponse: HttpResponse.Found <-
          responseOrError match {
            case Right(found: HttpResponse.Found) => ZIO.succeed(found)
            case Right(HttpResponse.NotFound)     => ZIO.succeed(HttpResponse.fromHttpCode.`404`)
            case Left(error) =>
              for {
                (specifiedResponseCode, errors) <- Handler.getHttpCodeAndErrors(error)
                responseCode =
                  specifiedResponseCode match {
                    case Some(responseCode) => responseCode
                    case None =>
                      if (errors.forall(_.isInstanceOf[HError.UserError])) HttpCode.`400`
                      else HttpCode.`500`
                  }
                _ <- ZIO.foreachDiscard(errors.toList) { e =>
                  Logger.log.error(e.fullInternalMessage, "error-type" -> e.getClass.getName) *>
                    Logger.log.debug(e.fullInternalMessageWithTrace, "error-type" -> e.getClass.getName)
                }
              } yield HttpResponse(errors.toList.map(_.userMessage.show(ifHidden)).toJson, responseCode)
          }

        _ <-
          ZIO
            .hAttempt {
              val headers = exchange.getResponseHeaders
              foundResponse.headers.foreach { (k, v) => headers.set(k, v) }
              foundResponse.cookies.reverse.foreach { c => headers.add("Set-Cookie", c.cookieString) }
            }
            .mapError(HError.SystemFailure("Unable to write response cookies", _))
        _ <- HttpResponse.Return.`return`(
          foundResponse.`return`,
          responseBody,
          bodyLength => ZIO.hAttempt(exchange.sendResponseHeaders(foundResponse.code.code, bodyLength)).mapError(HError.SystemFailure("Unable to write response headers", _)),
        )
      } yield ()

    Unsafe.unsafe { implicit unsafe =>
      serverRuntime.unsafe.run {
        ZIO.scoped {
          Logger
            .addContext("request-id" -> requestId) {
              effect
                .provideSomeLayer[HarnessEnv & ServerEnv & Scope](builtInReqLayer ++ reqLayer)
                .trace("HTTP Request Handler", Logger.LogLevel.Detailed, "path" -> exchange.getRequestURI.getPath)
            }
            .dumpErrorsAndContinue
        }
      }
    }
  }

}
object Handler {

  private def getHttpCodeAndErrors(error: HError): URIO[Logger, (Option[HttpCode], NonEmptyList[HError.Single])] = {
    def rec(error: HError): (List[HttpCode], NonEmptyList[HError.Single]) =
      error match {
        case ErrorWithHTTPCode(httpCode, child) =>
          val (childCodes, childErrors) = rec(child)
          (httpCode :: childCodes, childErrors)
        case error: HError.???    => (HttpCode.NotImplemented :: Nil, NonEmptyList.one(error))
        case error: HError.Single => (Nil, NonEmptyList.one(error))
        case HError.Multiple(children) =>
          val mapped = children.map(rec)
          (mapped.toList.flatMap(_._1), mapped.flatMap(_._2))
      }

    val (codes, errors) = rec(error)
    codes.distinct match {
      case Nil         => ZIO.succeed((None, errors))
      case code :: Nil => ZIO.succeed((code.some, errors))
      case codes =>
        val maxCode = codes.max
        Logger.log.warning(s"Found multiple codes ${codes.sorted.map(c => s"'$c'").mkString("[", ", ", "]")}, selecting max ($maxCode)").as((maxCode.some, errors))
    }
  }

}
