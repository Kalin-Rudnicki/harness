package harness.archive.domain.model.helpers

import harness.archive.api.model as Api
import harness.zio.*
import java.time.OffsetDateTime

object Misc {

  def keepUntilEpochMillis(durationMap: Api.app.DurationMap, logLevel: Logger.LogLevel, offsetDateTime: OffsetDateTime): Long =
    offsetDateTime.plus(durationMap.getDuration(logLevel)).toInstant.toEpochMilli

  def keepUntilEpochMillis(durationMap: Api.app.DurationMap, logLevel: Option[Logger.LogLevel], offsetDateTime: OffsetDateTime): Long =
    offsetDateTime.plus(durationMap.getDuration(logLevel)).toInstant.toEpochMilli

}
