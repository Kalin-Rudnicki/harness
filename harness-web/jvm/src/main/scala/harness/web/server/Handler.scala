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
    val builtInReqLayer: HTaskLayer[BuiltInRequestEnv] =
      ZLayer.fromZIO(ZIO.hAttempt(HttpRequest.read(exchange))) ++
        Scope.default

    val effect: SHRIO[ServerEnv & BuiltInRequestEnv & ReqEnv, Unit] =
      for {
        req <- ZIO.service[HttpRequest]
        ifHidden <- ZIO.service[HError.UserMessage.IfHidden]
        _ <- Logger.log.info(s"received ${req.method.method} request @ '${req.path.mkString("/", "/", "")}'")
        response1 <- route(req.method, req.path).either
        response2 <-
          response1 match {
            case Right(found: HttpResponse.Found) => ZIO.succeed(found)
            case Right(HttpResponse.NotFound)     => ZIO.succeed(HttpResponse("Not Found", HttpCode.`404`))
            case Left(error) =>
              val errors = error.toNel
              val allErrorsAreUserErrors = errors.forall(_.isInstanceOf[HError.UserError])
              val resp = HttpResponse(errors.toList.map(_.userMessage.show(ifHidden)).toJson, if (allErrorsAreUserErrors) HttpCode.`400` else HttpCode.`500`) // TODO (KR) :
              ZIO.foreachDiscard(errors.toList)(Logger.logHError.error(_)).as(resp)
          }
        responseBody = exchange.getResponseBody
        _ <- ZIO.hAttempt {
          val headers = exchange.getResponseHeaders
          response2.headers.foreach { (k, v) => headers.set(k, v) }
          response2.cookies.reverse.foreach { c => headers.add("Set-Cookie", c.cookieString) }
        }
        _ <- ZIO.hAttempt(exchange.sendResponseHeaders(response2.code.code, response2.length))
        _ <- ZIO.hAttempt(response2.write(responseBody))
        _ <- ZIO.hAttempt(responseBody.close()) // TODO (KR) : have as finalizer?
      } yield ()

    Unsafe.unsafe { implicit unsafe =>
      serverRuntime.unsafe.run {
        ZIO.scoped {
          Logger
            .addContext("request-id" -> UUID.randomUUID()) {
              effect.hLogDuration("HTTP Request Handler", Logger.LogLevel.Detailed)
            }
            .provideSomeLayer[HarnessEnv & ServerEnv & Scope](builtInReqLayer ++ reqLayer)
            .dumpErrorsAndContinue
        }
      }
    }
  }

}
