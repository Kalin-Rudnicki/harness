package harness.zio

import harness.core.*
import scala.reflect.{ensureAccessible, ClassTag}
import zio.json.*

extension (self: JsonCodec.type) {

  def `enum`[E <: Enum[E], Enc](implicit ec: JsonCodec[Enc], ewe: Enum.WithEnc[E, Enc], ct: ClassTag[E]): JsonCodec[E] =
    ec.transformOrFail(
      e => ewe.decode(e).toRight(s"Invalid ${ct.runtimeClass.getSimpleName}: $e"),
      ewe.encode,
    )

  def fromHarnessStringEncoderAndDecoder[T](implicit encoder: StringEncoder[T], decoder: StringDecoder[T]): JsonCodec[T] =
    JsonCodec.string.transformOrFail(
      decoder.decode,
      encoder.encode,
    )

}
