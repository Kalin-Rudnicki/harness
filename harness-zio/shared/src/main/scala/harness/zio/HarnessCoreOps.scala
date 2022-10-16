package harness.zio

import cats.data.NonEmptyList
import cats.syntax.either.*
import harness.core.*
import zio.json.*

extension (self: StringEncoder.type) {
  def fromJsonEncoder[T: JsonEncoder]: StringEncoder[T] = JsonEncoder[T].encodeJson(_, None).toString
}

extension (self: StringDecoder.type) {
  def fromJsonDecoder[T: JsonDecoder]: StringDecoder[T] = JsonDecoder[T].decodeJson(_).leftMap(NonEmptyList.one)
}
