package harness.kafka

import cats.data.NonEmptyList
import harness.core.*
import harness.kafka.error.DecodingFailure
import zio.*
import zio.json.*
import zio.kafka.serde.*

object HSerdes {

  def stringEncoded[A: StringEncoder: StringDecoder]: Serde[Any, A] =
    Serde.string.inmapM { string =>
      ZIO.fromEither(StringDecoder[A].decodeAccumulating(string)).mapError(DecodingFailure(_))
    } { a => ZIO.succeed(StringEncoder[A].encode(a)) }

  def jsonEncoded[A: JsonEncoder: JsonDecoder]: Serde[Any, A] =
    Serde.string.inmapM { string =>
      ZIO.fromEither(string.fromJson[A]).mapError(e => DecodingFailure(NonEmptyList.one(e)))
    } { a => ZIO.succeed(a.toJson) }

}
