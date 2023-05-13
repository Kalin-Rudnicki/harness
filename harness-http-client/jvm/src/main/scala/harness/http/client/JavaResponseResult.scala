package harness.http.client

import harness.core.*
import harness.web.*
import harness.zio.*
import zio.*

final case class JavaResponseResult(
    responseCode: HttpCode,
    _headers: Map[String, List[String]],
    _contentLength: Option[Long],
    body: JavaClient.ResponseT,
) extends HttpResponse.Result[JavaClient.ResponseT] { self =>

  override def bodyAsStringImpl(body: JavaClient.ResponseT): HRIO[Logger, String] =
    for {
      cl <- self.contentLengthInt
      getBytes = cl match {
        case Some(cl) => Logger.log.debug(s"Reading body with content length of ${cl.toStringCommas}") *> ZIO.hAttempt { self.body.readNBytes(cl) }
        case None     => Logger.log.warning("Reading body without content length") *> ZIO.hAttempt { self.body.readAllBytes() }
      }
      bytes: Array[Byte] <- getBytes.mapError(HError.SystemFailure("Error getting bytes for response body", _))
    } yield new String(bytes)

  override def forwardBodyToPath(path: Path): HTask[Long] =
    ZIO.scoped {
      path.outputStream.flatMap { os =>
        ZIO.hAttempt { self.body.transferTo(os) }.mapError(HError.SystemFailure(s"Error forwarding response body to path $path", _))
      }
    }

}
