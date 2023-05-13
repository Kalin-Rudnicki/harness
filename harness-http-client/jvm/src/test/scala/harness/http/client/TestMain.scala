package harness.http.client

import harness.cli.*
import harness.core.*
import harness.zio.*
import java.io.InputStream
import zio.*

object TestMain extends ExecutableApp {

  override val executable: Executable =
    Executable
      .withParser(Parser.unit)
      .withLayer(JavaClient.layer)
      .withEffect {
        for {
          _ <- Logger.log.info("=====| TestMain |=====")
          reqs: List[HttpRequest[JavaClient.RequestT]] =
            List(
              // HttpRequest.get("https://www.google.com/").withNoBody,
              HttpRequest.get("https://www.google.com/wtf").withNoBody,
              // HttpRequest.get("http://localhost:3000/page/home").withNoBody,
              // HttpRequest.post("http://localhost:3000/page/home").withNoBody,
              // HttpRequest.post("http://localhost:3000/api/ofx/upload-transactions").withNoBody,
            )
          _ <- ZIO.foreachDiscard(reqs) { req =>
            val effect: HRIO[HttpClient.ClientT & Logger & Telemetry & Scope, Unit] =
              for {
                // response <- HttpClient.send(req)
                // _ <- response.show.flatMap(Logger.log.info(_))
                // _ <- response.decodedBody[String].flatMap(Logger.log.info(_))
                _ <- req.response.decodedBody[String].flatMap(Logger.log.important(_))
              } yield ()

            ZIO
              .scoped { effect }
              .collapseCause
              .foldZIO(
                errors => ZIO.foreachDiscard(errors.toNel.toList) { error => Logger.log.error(error.fullInternalMessageWithTrace) },
                _ => ZIO.unit,
              )
          }
        } yield ()
      }

}
