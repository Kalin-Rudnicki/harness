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
    reqLayer: ZLayer[ServerEnv & Scope, NonEmptyList[HError], ReqEnv],
    route: Route[ServerEnv & ReqEnv],
) extends HttpHandler {

  override def handle(exchange: HttpExchange): Unit = {
    val builtInReqLayer: Layer[NonEmptyList[HError], BuiltInRequestEnv] =
      ZLayer.fromZIO(ZIO.hAttemptNel("Error parsing http request")(HttpRequest.read(exchange))) ++
        Scope.default

    val effect: SHRION[ServerEnv & BuiltInRequestEnv & ReqEnv, Unit] =
      for {
        req <- ZIO.service[HttpRequest]
        _ <- Logger.log.info(s"received ${req.method.method} request @ '${req.path.mkString("/", "/", "")}'")
        response1 <- route(req.method, req.path).either
        response2 <-
          response1 match {
            case Right(found: HttpResponse.Found) => ZIO.succeed(found)
            case Right(HttpResponse.NotFound)     => ZIO.succeed(HttpResponse("Not Found", HttpCode.`404`))
            case Left(errors) =>
              val allErrorsAreUserErrors = errors.forall(_.isInstanceOf[HError.UserError])
              val resp = HttpResponse(errors.toList.map(_.userMessage).toJson, if (allErrorsAreUserErrors) HttpCode.`400` else HttpCode.`500`)
              ZIO.foreachDiscard(errors.toList)(Logger.logHError.error(_)).as(resp)
          }
        responseBody = exchange.getResponseBody
        _ <- ZIO.hAttemptNel("Error setting response headers") {
          val headers = exchange.getResponseHeaders
          response2.headers.foreach { (k, v) => headers.set(k, v) }
          response2.cookies.reverse.foreach { c => headers.add("Set-Cookie", c.cookieString) }
        }
        _ <- ZIO.hAttemptNel("Error writing response headers")(exchange.sendResponseHeaders(response2.code.code, response2.length))
        _ <- ZIO.hAttemptNel("Error writing response body")(response2.write(responseBody))
        _ <- ZIO.hAttemptNel("Error closing response body")(responseBody.close())
      } yield ()

    Unsafe.unsafe { implicit unsafe =>
      serverRuntime.unsafe.run {
        ZIO.scoped {
          Logger.addContext("request-id" -> UUID.randomUUID())(effect).provideSomeLayer[HarnessEnv & ServerEnv & Scope](builtInReqLayer ++ reqLayer).dumpErrorsAndContinueNel
        }
      }
    }
  }

}
