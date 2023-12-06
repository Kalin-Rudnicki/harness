package harness.http.client

import cats.data.NonEmptyList
import harness.core.*
import harness.zio.*
import harness.zio.ZIOJsonInstances.catsNelJsonCodec
import zio.*
import zio.json.*

trait ResponseOps[GetResponseR, GetBodyR, ResponseT] { self =>

  protected def getResponse: HRIO[GetResponseR & Scope, HttpResponse[ResponseT]]

  // =====| Body |=====

  def bodyAsString: HRIO[GetResponseR & GetBodyR & Logger, String] = ZIO.scoped { self.getResponse.flatMap(_.bodyAsString) }

  final def rawDecodedBody[T](implicit decoder: StringDecoder[T]): HRIO[GetResponseR & GetBodyR & Logger, T] =
    self.bodyAsString.flatMap { string => ZIO.eitherNelToUserErrors(decoder.decodeAccumulating(string)) }
  final def rawJsonBody[T: JsonDecoder]: HRIO[GetResponseR & GetBodyR & Logger, T] =
    self.rawDecodedBody[T](StringDecoder.fromJsonDecoder[T])

  final def decodedBody[T](implicit decoder: StringDecoder[T]): HRIO[GetResponseR & GetBodyR & Logger, T] =
    ZIO.scoped {
      self.getResponse.flatMap { response =>
        response.bodyAsString.flatMap { string =>
          if (response.responseCode.is2xx) ZIO.eitherNelToUserErrors(decoder.decodeAccumulating(string))
          else if (response.responseCode.is4xxOr5xx)
            string.fromJson[NonEmptyList[String]] match {
              case Right(errors) => ZIO.fail(HError(errors.map(HError.UserError(_, "Error from HTTP response"))))
              case Left(_)       => ZIO.fail(HError.UserError(string, "Error from HTTP response"))
            }
          else
            Logger.log.warning("Received HTTP response that is not 2xx, 4xx, or 5xx.") *>
              (decoder.decodeAccumulating(string) match {
                case Right(value) => ZIO.succeed(value)
                case Left(errors) =>
                  Logger.log.warning(s"Was unable to decode: ${errors.toList.mkString("[", ", ", "]")}, failing...") *>
                    ZIO.fail(HError.UserError(string, "Error from HTTP response (response code was not 2xx, 4xx, or 5xx)"))
              })
        }
      }
    }
  final def jsonBody[T: JsonDecoder]: HRIO[GetResponseR & GetBodyR & Logger, T] =
    self.decodedBody[T](StringDecoder.fromJsonDecoder[T])

  // =====| Other |=====

  final def unit2xx: HRIO[GetResponseR & Logger, Unit] =
    ZIO.scoped {
      self.getResponse.flatMap { response =>
        if (response.responseCode.is2xx) ZIO.unit
        else
          response.bodyAsString.flatMap { body => ZIO.fail(HError.UserError(body, s"Expected 2xx response code, but got: ${response.responseCode}")) }
      }
    }

}
