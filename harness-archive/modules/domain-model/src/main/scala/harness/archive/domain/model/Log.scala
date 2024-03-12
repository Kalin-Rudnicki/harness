package harness.archive.domain.model

import harness.archive.api.model as Api
import harness.archive.domain.model.helpers.Misc
import harness.zio.Logger
import java.time.OffsetDateTime

final case class Log(
    id: Api.log.LogId,
    appId: Api.app.AppId,
    logLevel: Option[Logger.LogLevel],
    message: String,
    context: Map[String, String],
    dateTime: OffsetDateTime,
    epochMS: Long,
    keepUntilEpochMS: Long,
)
object Log {

  def create(app: App, log: Api.log.Upload.Log): Log =
    Log(
      id = Api.log.LogId.gen,
      appId = app.id,
      logLevel = log.logLevel,
      message = log.message,
      context = log.context,
      dateTime = log.dateTime,
      epochMS = log.dateTime.toInstant.toEpochMilli,
      keepUntilEpochMS = Misc.keepUntilEpochMillis(app.logDurationMap, log.logLevel, log.dateTime),
    )

}
