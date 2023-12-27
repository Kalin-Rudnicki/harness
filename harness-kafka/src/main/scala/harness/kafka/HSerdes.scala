package harness.kafka

import harness.core.*
import harness.zio.*
import zio.*
import zio.json.*
import zio.kafka.serde.*

object HSerdes {

  def stringEncoded[A: StringEncoder: StringDecoder]: Serde[Any, A] =
    Serde.string.inmapM { string =>
      ZIO.eitherNelToInternalDefects(StringDecoder[A].decodeAccumulating(string))
    } { a => ZIO.succeed(StringEncoder[A].encode(a)) }

  def jsonEncoded[A: JsonEncoder: JsonDecoder]: Serde[Any, A] =
    Serde.string.inmapM {
      _.fromJson[A] match {
        case Right(value) => ZIO.succeed(value)
        case Left(error)  => ZIO.fail(HError.InternalDefect(s"Unable to decode kafka bytes to json: $error"))
      }
    } { a => ZIO.succeed(a.toJson) }

}
