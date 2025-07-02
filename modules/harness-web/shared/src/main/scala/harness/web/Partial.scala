package harness.web

import cats.syntax.option.*
import zio.json.*
import zio.json.internal.{RetractReader, Write}

sealed trait Partial[+T] {
  final def toOption: Option[T] =
    this match {
      case Partial.Specified(value) => value.some
      case Partial.Unspecified      => None
    }
  final def specifiedOr[T2 >: T](or: => T2): T2 =
    this match {
      case Partial.Specified(value) => value
      case Partial.Unspecified      => or
    }
}
object Partial {

  // =====| Types |=====

  final case class Specified[+T](value: T) extends Partial[T]
  case object Unspecified extends Partial[Nothing]

  // =====| Helpers |=====

  given convertToSpecified: [T] => Conversion[T, Partial.Specified[T]] = Partial.Specified(_)

  // =====| Json Encoder/Decoder/Codec |=====

  /**
    * Different options for encoding/decoding a Partial.
    * You need to import one of these in order to have generic derivation of types containing a Partial[_].
    *
    * The naming convention is: `encoder / decoder`, and `x` mean `x / x`.
    */

  object flat {

    implicit def jsonEncoder[T](implicit tEncoder: JsonEncoder[T]): JsonEncoder[Partial[T]] =
      new JsonEncoder[Partial[T]] {
        override def unsafeEncode(a: Partial[T], indent: Option[Int], out: Write): Unit =
          a match {
            case Specified(value) => tEncoder.unsafeEncode(value, indent, out)
            case Unspecified      =>
          }
        override def isNothing(a: Partial[T]): Boolean =
          a match {
            case Specified(_) => false
            case Unspecified  => true
          }
      }

    implicit def jsonDecoder[T](implicit tDecoder: JsonDecoder[T]): JsonDecoder[Partial[T]] =
      new JsonDecoder[Partial[T]] { self =>
        override def unsafeDecodeMissing(trace: List[JsonError]): Partial[T] =
          Partial.Unspecified
        override def unsafeDecode(trace: List[JsonError], in: RetractReader): Partial[T] =
          Partial.Specified(tDecoder.unsafeDecode(trace, in))
      }

    implicit def jsonCodec[T: {JsonEncoder, JsonDecoder}]: JsonCodec[Partial[T]] = JsonCodec(jsonEncoder[T], jsonDecoder[T])

  }

  object auto {
    implicit def jsonEncoder[T: JsonEncoder]: JsonEncoder[Partial[T]] = DeriveJsonEncoder.gen[Partial[T]]
    implicit def jsonDecoder[T: JsonDecoder]: JsonDecoder[Partial[T]] = DeriveJsonDecoder.gen[Partial[T]]
    implicit def jsonCodec[T: {JsonEncoder, JsonDecoder}]: JsonCodec[Partial[T]] = JsonCodec(jsonEncoder[T], jsonDecoder[T])
  }

  // TODO (KR) : If `zio-json` changes to defer `unsafeDecodeMissing` on `orElse`, then this can be simplified to just `flat.jsonDecoder[T] <> auto.jsonDecoder[T]`
  private def flatOrAutoDecoder[T: JsonDecoder]: JsonDecoder[Partial[T]] = {
    val parent: JsonDecoder[Partial[T]] = auto.jsonDecoder[T] <> flat.jsonDecoder[T]

    new JsonDecoder[Partial[T]] {
      override def unsafeDecode(trace: List[JsonError], in: RetractReader): Partial[T] = parent.unsafeDecode(trace, in)
      override def unsafeDecodeMissing(trace: List[JsonError]): Partial[T] = Partial.Unspecified
    }
  }

  object `flat / flatOrAuto` {
    export flat.jsonEncoder
    implicit def jsonDecoder[T: JsonDecoder]: JsonDecoder[Partial[T]] = flatOrAutoDecoder[T]
    implicit def jsonCodec[T: {JsonEncoder, JsonDecoder}]: JsonCodec[Partial[T]] = JsonCodec(jsonEncoder[T], jsonDecoder[T])
  }

  object `auto / flatOrAuto` {
    export auto.jsonEncoder
    implicit def jsonDecoder[T: JsonDecoder]: JsonDecoder[Partial[T]] = flatOrAutoDecoder[T]
    implicit def jsonCodec[T: {JsonEncoder, JsonDecoder}]: JsonCodec[Partial[T]] = JsonCodec(jsonEncoder[T], jsonDecoder[T])
  }

}
