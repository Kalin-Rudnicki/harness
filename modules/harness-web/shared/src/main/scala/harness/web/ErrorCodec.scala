package harness.web

import harness.core.*
import harness.zio.EncodedThrowable
import zio.*
import zio.json.*

trait ErrorCodec[E] {
  val encode: E => String
  val decode: String => Either[String, E]
}
object ErrorCodec {

  def apply[E](implicit ec: ErrorCodec[E]): ErrorCodec[E] = ec

  def jsonCodec[E: JsonEncoder: JsonDecoder]: ErrorCodec[E] =
    new ErrorCodec[E] {
      override val encode: E => String = _.toJson
      override val decode: String => Either[String, E] = _.fromJson[E]
    }

  def stringCodec[E: StringEncoder: StringDecoder]: ErrorCodec[E] =
    new ErrorCodec[E] {
      override val encode: E => String = StringEncoder[E].encode
      override val decode: String => Either[String, E] = StringDecoder[E].decode
    }

  val forThrowable: ErrorCodec[Throwable] =
    ErrorCodec.jsonCodec[Throwable](using EncodedThrowable.throwableJsonCodec.encoder, EncodedThrowable.throwableJsonCodec.decoder)

}
