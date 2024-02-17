package harness.web

import harness.core.*
import harness.web.error.UnexpectedResponseBody
import harness.zio.EncodedThrowable
import zio.json.*

trait ErrorCodec[E] {
  val encode: E => String
  protected val decodeInternal: String => Either[String, E]
  val convertUnexpected: Throwable => E

  final val decode: String => E = body => decodeInternal(body).fold(decodeMsg => convertUnexpected(UnexpectedResponseBody(body, decodeMsg)), identity)
}
object ErrorCodec {

  def apply[E](implicit ec: ErrorCodec[E]): ErrorCodec[E] = ec

  final class Builder[E](
      encode: E => String,
      decode: String => Either[String, E],
  ) { self =>

    def convertUnexpected(_convertUnexpected: Throwable => E): ErrorCodec[E] =
      new ErrorCodec[E] {
        override val encode: E => String = self.encode
        override protected val decodeInternal: String => Either[String, E] = self.decode
        override val convertUnexpected: Throwable => E = _convertUnexpected
      }

  }

  def jsonCodec[E: JsonEncoder: JsonDecoder]: ErrorCodec.Builder[E] =
    new ErrorCodec.Builder[E](
      encode = _.toJson,
      decode = _.fromJson[E],
    )

  def stringCodec[E: StringEncoder: StringDecoder]: ErrorCodec.Builder[E] =
    ErrorCodec.Builder[E](
      encode = StringEncoder[E].encode,
      decode = StringDecoder[E].decode,
    )

  // TODO (KR) : use json encoded throwable?
  val forThrowable: ErrorCodec[Throwable] =
    ErrorCodec
      .jsonCodec[Throwable](using EncodedThrowable.throwableJsonCodec.encoder, EncodedThrowable.throwableJsonCodec.decoder)
      .convertUnexpected(identity)

}
