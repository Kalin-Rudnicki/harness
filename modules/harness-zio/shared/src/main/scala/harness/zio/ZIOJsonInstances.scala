package harness.zio

import cats.data.NonEmptyList
import cats.syntax.either.*
import harness.core.*
import zio.json.*

object ZIOJsonInstances {

  implicit def catsNelJsonCodec[A: JsonCodec]: JsonCodec[NonEmptyList[A]] =
    JsonCodec
      .list[A]
      .transformOrFail(
        {
          case head :: tail => NonEmptyList(head, tail).asRight
          case Nil          => "NonEmptyList has 0 elements".asLeft
        },
        _.toList,
      )

  implicit def fieldDecoderFromStringDecoder[T](implicit sd: StringDecoder[T]): JsonFieldDecoder[T] =
    JsonFieldDecoder.string.mapOrFail(sd.decode)

  implicit def fieldEncoderFromStringEncoder[T](implicit se: StringEncoder[T]): JsonFieldEncoder[T] =
    JsonFieldEncoder.string.contramap(se.encode)

  implicit val throwableJsonCodec: JsonCodec[Throwable] =
    EncodedThrowable.throwableJsonCodec

}
