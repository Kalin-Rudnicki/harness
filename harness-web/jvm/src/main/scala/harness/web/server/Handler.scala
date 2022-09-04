package harness.web.server

import cats.data.NonEmptyList
import com.sun.net.httpserver.*
import harness.core.*
import harness.zio.*
import zio.*

final case class Handler[R](runtime: Runtime[HarnessEnv & ServerEnv & R], route: Route[R]) extends HttpHandler {

  override def handle(exchange: HttpExchange): Unit = {
    val requestLayer: Layer[NonEmptyList[HError], RequestEnv] =
      ZLayer.fromZIO(ZIO.hAttemptNel("Error parsing http request")(HttpRequest.read(exchange)))

    val effect: SHRION[ServerEnv & RequestEnv & R, Unit] =
      for {
        req <- ZIO.service[HttpRequest]
        _ <- Logger.log.info(s"received ${req.method.method} request @ '${req.path.mkString("/", "/", "")}'")
        response1 <- route(req.method, req.path).either
        response2 =
          response1 match {
            case Right(found: HttpResponse.Found) => found
            case Right(HttpResponse.NotFound)     => HttpResponse("Not Found", HttpCode.`404`)
            case Left(errors) =>
              val allErrorsAreUserErrors = errors.forall(_.isInstanceOf[HError.UserError])
              HttpResponse(errors.toList.map(_.userMessage).mkString("\n"), if (allErrorsAreUserErrors) HttpCode.`400` else HttpCode.`500`)
          }
        responseBody = exchange.getResponseBody
        _ <- ZIO.hAttemptNel("Error setting response headers") {
          val headers = exchange.getResponseHeaders
          response2.headers.foreach { (k, v) => headers.set(k, v) }
        }
        _ <- ZIO.hAttemptNel("Error writing response headers")(exchange.sendResponseHeaders(response2.code.code, response2.length))
        _ <- ZIO.hAttemptNel("Error writing response body")(response2.write(responseBody))
        _ <- ZIO.hAttemptNel("Error closing response body")(responseBody.close())
      } yield ()

    Unsafe.unsafe {
      runtime.unsafe.run {
        effect.provideSomeLayer(requestLayer).dumpErrorsAndContinueNel
      }
    }
  }

}
