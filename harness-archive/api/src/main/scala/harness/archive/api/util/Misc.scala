package harness.archive.api.util

import harness.archive.api.db.model as M
import harness.archive.api.service.storage.*
import harness.archive.model as D
import harness.core.*
import harness.http.server.*
import harness.sql.*
import harness.zio.*
import java.time.OffsetDateTime
import zio.*

object Misc {

  val warnUserPermissions: URIO[Logger, Unit] =
    Logger.log.warning("TODO : User Permissions")

  private def makeApp(appName: String): M.App.Identity =
    new M.App.Identity(
      M.App.Id.gen,
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

  def appByName(appName: String): HRIO[AppStorage & JDBCConnection & Logger & Telemetry & HttpRequest, M.App.Identity] =
    AppStorage.byName(appName).someOrFail(HError.UserError(s"No such app with name '$appName'"))

  def getOrCreateApp(appName: String): HRIO[AppStorage & JDBCConnection & Logger & Telemetry & HttpRequest, M.App.Identity] =
    AppStorage.byName(appName).flatMap {
      case Some(app) => ZIO.succeed(app)
      case None =>
        val dbApp = makeApp(appName)
        AppStorage.insert(dbApp).as(dbApp)
    }

  def getOrCreateApps(appNames: Set[String]): HRIO[AppStorage & JDBCConnection & Logger & Telemetry & HttpRequest, Map[String, M.App.Identity]] =
    for {
      existingApps <- AppStorage.selectAll
      existingAppMap = existingApps.map(a => (a.name, a)).toMap

      appsToBeInserted = Chunk.fromIterable(appNames.filterNot(existingAppMap.contains)).map(makeApp)

      _ <- ZIO.when(appsToBeInserted.nonEmpty) { AppStorage.insertAll(appsToBeInserted) }
    } yield existingAppMap ++ appsToBeInserted.map(a => (a.name, a)).toMap

  def keepUntilEpochMillis(durationMap: D.app.DurationMap, logLevel: Logger.LogLevel, offsetDateTime: OffsetDateTime): Long =
    offsetDateTime.plus(durationMap.getDuration(logLevel)).toInstant.toEpochMilli

  def keepUntilEpochMillis(durationMap: D.app.DurationMap, logLevel: Option[Logger.LogLevel], offsetDateTime: OffsetDateTime): Long =
    offsetDateTime.plus(durationMap.getDuration(logLevel)).toInstant.toEpochMilli

}
