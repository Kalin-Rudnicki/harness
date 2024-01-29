package harness.http.client

import cats.data.NonEmptyList
import harness.core.*
import harness.zio.*
import harness.zio.ZIOJsonInstances.catsNelJsonCodec
import zio.*
import zio.json.*

object ResponseOps {

  private def decodeHError(response: HttpResponse[String]): HError = {
    val internalMessage = s"Error from HTTP response (${response.responseCode})"
    response.body.fromJson[NonEmptyList[String]] match {
      case Right(errors) => HError(errors.map(HError.UserError(_, internalMessage)))
      case Left(_)       => HError.UserError(response.body, internalMessage)
    }
  }

  trait Builder1[GetResponseR, ResponseT] extends Builder2[GetResponseR, ResponseT, HError] { self =>

    override protected def decodeError(response: HttpResponse[String]): HError = decodeHError(response)

    private def withDecodeError[ErrorT >: HError <: AnyHError](f: HttpResponse[String] => ErrorT): Builder2[GetResponseR, ResponseT, ErrorT] =
      new Builder2[GetResponseR, ResponseT, ErrorT] {
        override protected def getResponse: HRIO[GetResponseR & Scope, HttpResponse[ResponseT]] = self.getResponse
        override protected def decodeError(response: HttpResponse[String]): ErrorT = f(response)
      }

    final def hErrorResponse: Builder2[GetResponseR, ResponseT, HError] =
      self.withDecodeError(decodeHError)
    final def hErrorOrResponse[E: StringDecoder]: Builder2[GetResponseR, ResponseT, HErrorOr[E]] =
      self.withDecodeError { response =>
        StringDecoder[E].decodeAccumulating(response.body) match {
          case Right(error) => HError.Or(error, HError.UserError(s"Error from HTTP response (${response.responseCode})"))
          case Left(_)      => decodeHError(response)
        }
      }
    final def hErrorOrJsonResponse[E: JsonDecoder]: Builder2[GetResponseR, ResponseT, HErrorOr[E]] =
      self.hErrorOrResponse[E](using StringDecoder.fromJsonDecoder[E])

  }

  trait Builder2[GetResponseR, ResponseT, ErrorT >: HError <: AnyHError] { self =>

    protected def getResponse: HRIO[GetResponseR & Scope, HttpResponse[ResponseT]]
    protected def decodeError(response: HttpResponse[String]): ErrorT

    private def decodeErrorInternal(response: HttpResponse[String]): URIO[Logger, ErrorT] =
      Logger.log.warning(s"Attempting to decode error for non-(4xx/5xx) error code: ${response.responseCode}").unless(response.responseCode.is4xxOr5xx).as(decodeError(response))

    private def attemptToDecode[T](f: HttpResponse[String] => HRIO[Logger, Option[T]]): ZIO[GetResponseR & Logger, ErrorT, T] =
      getResponseStringBody.flatMap { response =>
        f(response).someOrElseZIO(decodeErrorInternal(response).flip)
      }

    // =====|  |=====

    final def getResponseStringBody: HRIO[GetResponseR & Logger, HttpResponse[String]] =
      ZIO.scoped { self.getResponse.flatMap(_.toResponseStringBody) }

    def stringBody: ZIO[GetResponseR & Logger, ErrorT, String] =
      attemptToDecode { response => ZIO.succeed(response.body).when(response.responseCode.is2xx) }

    def encodedBody[T: StringDecoder]: ZIO[GetResponseR & Logger, ErrorT, T] =
      attemptToDecode { response => ZIO.eitherNelToInternalDefects(StringDecoder[T].decodeAccumulating(response.body)).when(response.responseCode.is2xx) }
    def jsonBody[T: JsonDecoder]: ZIO[GetResponseR & Logger, ErrorT, T] =
      self.encodedBody[T](using StringDecoder.fromJsonDecoder[T])

    def unit2xx: ZIO[GetResponseR & Logger, ErrorT, Unit] =
      attemptToDecode { response => ZIO.unit.when(response.responseCode.is2xx) }

    // =====|  |=====

    final def forwardBodyToPath(path: Path): HRIO[GetResponseR, Long] =
      ZIO.scoped {
        self.getResponse.flatMap { response =>
          response.result.ops.forwardBodyToPath(path, response.body)
        }
      }

  }

}
