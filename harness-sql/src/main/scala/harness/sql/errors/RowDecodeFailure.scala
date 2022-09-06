package harness.sql.errors

import cats.data.NonEmptyList

final case class RowDecodeFailure(errors: NonEmptyList[String])
    extends Throwable(
      s"Error decoding row:${errors.toList.map(e => s"\n  - $e").mkString}",
    )
