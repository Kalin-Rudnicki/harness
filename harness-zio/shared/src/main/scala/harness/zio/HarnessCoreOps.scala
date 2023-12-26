package harness.zio

import cats.data.NonEmptyList
import cats.syntax.either.*
import harness.core.*
import zio.json.*

implicit class StringEncoderOps (self: StringEncoder.type) {
  def fromJsonEncoder[T: JsonEncoder]: StringEncoder[T] = JsonEncoder[T].encodeJson(_, None).toString
}

implicit class StringDecoderOps (self: StringDecoder.type) {
  def fromJsonDecoder[T: JsonDecoder]: StringDecoder[T] = JsonDecoder[T].decodeJson(_).leftMap(NonEmptyList.one)
}
