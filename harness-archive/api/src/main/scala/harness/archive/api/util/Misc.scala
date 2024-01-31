package harness.archive.api.util

import harness.archive.api.db.model as M
import harness.archive.api.service.storage.*
import harness.archive.model as D
import harness.core.*
import harness.http.server.*
import harness.zio.*
import java.time.OffsetDateTime
import zio.*

object Misc {

  val warnUserPermissions: URIO[Logger, Unit] =
    Logger.log.warning("TODO : User Permissions")

  def makeApp(dbUser: M.User.Identity, appName: String): M.App.Identity =
    new M.App.Identity(
      M.App.Id.gen,
      dbUser.id,
      appName,
      D.app.DurationMap.make(5.minutes)(
        Logger.LogLevel.Trace -> 1.hour,
        Logger.LogLevel.Debug -> 6.hours,
        Logger.LogLevel.Info -> 1.day,
        Logger.LogLevel.Warning -> 2.days,
        Logger.LogLevel.Error -> 7.days,
      ),
      D.app.DurationMap.make(5.minutes)(
        Logger.LogLevel.Trace -> 6.hours,
        Logger.LogLevel.Debug -> 12.hours,
        Logger.LogLevel.Info -> 1.day,
        Logger.LogLevel.Warning -> 2.days,
        Logger.LogLevel.Error -> 7.days,
      ),
    )

  def appByName(dbUser: M.User.Identity, appName: String): HRIO[AppStorage & Logger & Telemetry & HttpRequest, M.App.Identity] =
    AppStorage.byName(dbUser.id, appName).someOrFail(HError.UserError(s"No such app with name '$appName' for user ${dbUser.show}"))

  def keepUntilEpochMillis(durationMap: D.app.DurationMap, logLevel: Logger.LogLevel, offsetDateTime: OffsetDateTime): Long =
    offsetDateTime.plus(durationMap.getDuration(logLevel)).toInstant.toEpochMilli

  def keepUntilEpochMillis(durationMap: D.app.DurationMap, logLevel: Option[Logger.LogLevel], offsetDateTime: OffsetDateTime): Long =
    offsetDateTime.plus(durationMap.getDuration(logLevel)).toInstant.toEpochMilli

}
