package harness.sql.errors

import cats.data.NonEmptyList
import harness.core.*

final case class RowDecodeFailure(errors: NonEmptyList[String])
    extends HError.Single(
      HError.UserMessage.hidden,
      s"Error decoding row:${errors.toList.map(e => s"\n  - $e").mkString}",
      Nil,
    )
