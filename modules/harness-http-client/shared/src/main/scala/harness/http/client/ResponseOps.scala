package harness.http.client

import cats.syntax.either.*
import harness.core.*
import harness.web.Constants.harnessInternalErrorHeader
import harness.web.ErrorCodec
import harness.zio.*
import zio.*
import zio.json.*
import java.util.Base64

object ResponseOps {

  trait Builder1[GetResponseR, ResponseT] extends Builder2[GetResponseR, ResponseT, Throwable] { self =>

    override protected val errorCodec: ErrorCodec[Throwable] = ErrorCodec.forThrowable

    final def withError[E: ErrorCodec]: Builder2[GetResponseR, ResponseT, E] =
      new Builder2[GetResponseR, ResponseT, E] {
        override protected def getResponse: RIO[GetResponseR & Scope, HttpResponse[ResponseT]] = self.getResponse
        override protected val errorCodec: ErrorCodec[E] = ErrorCodec[E]
      }

  }

  trait Builder2[GetResponseR, ResponseT, ErrorT] { self =>

    protected def getResponse: RIO[GetResponseR & Scope, HttpResponse[ResponseT]]
    protected val errorCodec: ErrorCodec[ErrorT]

    private def succeedOrDie[T](result: Either[String, T]): IO[Nothing, T] =
      result match {
        case Left(error)  => ZIO.dieMessage(s"Unable to decode response result:\n$error")
        case Right(value) => ZIO.succeed(value)
      }

    private def failOrDie(result: Either[String, ErrorT]): IO[ErrorT, Nothing] =
      result match {
        case Left(error)  => ZIO.dieMessage(s"Unable to decode error result:\n$error")
        case Right(value) => ZIO.fail(value)
      }

    private def attemptToDecode[T](f: String => Either[String, T]): ZIO[GetResponseR & Logger, ErrorT, T] =
      getResponseStringBody.orDie.flatMap { response =>
        ZIO.foreachDiscard(response.headers.getOrElse(harnessInternalErrorHeader, Nil)) { value =>
          Logger.log.debug(s"Harness internal error:\n${new String(Base64.getDecoder.decode(value))}")
        } *>
          (if (response.responseCode.is2xx)
             succeedOrDie(f(response.body))
           else if (response.responseCode.is4xxOr5xx)
             failOrDie(errorCodec.decode(response.body))
           else
             Logger.log.warning(s"Received non-(2xx/4xx/5xx) response: ${response.responseCode}") *>
               failOrDie(errorCodec.decode(response.body)))
      }

    // =====|  |=====

    final def getResponseStringBody: RIO[GetResponseR & Logger, HttpResponse[String]] =
      ZIO.scoped { self.getResponse.flatMap(_.toResponseStringBody) }

    final def stringBody: ZIO[GetResponseR & Logger, ErrorT, String] =
      attemptToDecode(_.asRight)

    final def encodedBody[T: StringDecoder]: ZIO[GetResponseR & Logger, ErrorT, T] =
      attemptToDecode(StringDecoder[T].decode)
    final def jsonBody[T: JsonDecoder]: ZIO[GetResponseR & Logger, ErrorT, T] =
      self.encodedBody[T](using StringDecoder.fromJsonDecoder[T])

    final def unit2xx: ZIO[GetResponseR & Logger, ErrorT, Unit] =
      attemptToDecode(_ => ().asRight)

    // =====|  |=====

    final def forwardBodyToPath(path: Path): RIO[GetResponseR, Long] =
      ZIO.scoped {
        self.getResponse.flatMap { response => response.result.ops.forwardBodyToPath(path, response.body) }
      }

  }

}
