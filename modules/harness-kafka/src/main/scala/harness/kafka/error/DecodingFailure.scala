package harness.kafka.error

import cats.data.NonEmptyList

final case class DecodingFailure(errors: NonEmptyList[String]) extends Throwable {
  override def getMessage: String =
    errors.toList match {
      case error :: Nil => s"Kafka encountered decoding failure: $error"
      case errors       => s"Kafka encountered decoding failures:${errors.map(e => s"\n  - $e").mkString}"
    }
}
