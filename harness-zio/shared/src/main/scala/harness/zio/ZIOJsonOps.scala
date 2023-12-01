package harness.zio

import cats.data.NonEmptyList
import cats.syntax.either.*
import harness.core.*
import scala.reflect.{ensureAccessible, ClassTag}
import zio.json.*

implicit class JsonEncoderOps (self: JsonEncoder.type) {

  def fromHarnessStringEncoder[T](implicit encoder: StringEncoder[T]): JsonEncoder[T] =
    JsonEncoder.string.contramap(encoder.encode)

}

implicit class JsonDecoderOps (self: JsonDecoder.type) {

  def fromHarnessStringDecoder[T](implicit decoder: StringDecoder[T]): JsonDecoder[T] =
    JsonDecoder.string.mapOrFail(decoder.decode)

}

implicit class JsonCodecOps (self: JsonCodec.type) {

  def `enum`[E <: Enum[E], Enc](implicit ec: JsonCodec[Enc], ewe: Enum.WithEnc[E, Enc], ct: ClassTag[E]): JsonCodec[E] =
    ec.transformOrFail(
      e => ewe.decode(e).toRight(s"Invalid ${ct.runtimeClass.getSimpleName}: $e"),
      ewe.encode,
    )

  def fromHarnessStringEncoderAndDecoder[T](implicit encoder: StringEncoder[T], decoder: StringDecoder[T]): JsonCodec[T] =
    JsonCodec(JsonEncoder.fromHarnessStringEncoder[T], JsonDecoder.fromHarnessStringDecoder[T])

}
