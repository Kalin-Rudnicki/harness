package harness.kafka.error

import cats.data.NonEmptyList

final case class DecodingFailure(errors: NonEmptyList[String]) extends Throwable
