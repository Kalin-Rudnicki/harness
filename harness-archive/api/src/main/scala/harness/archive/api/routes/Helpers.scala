package harness.archive.api.routes

import harness.archive.api.db.{model as M, queries as Q}
import harness.archive.model as D
import harness.core.*
import harness.http.server.*
import harness.sql.*
import harness.web.*
import harness.zio.*
import java.time.OffsetDateTime
import zio.*

private[routes] object Helpers {

  val SessionToken: String = "Archive-Session-Token"

  val userFromSessionOptional: HRIO[JDBCConnection & Logger & Telemetry & HttpRequest, Option[M.User.Identity]] =
    HttpRequest.cookie.find[String](Helpers.SessionToken).flatMap {
      case Some(tok) =>
        Q.User.fromSessionToken(tok).option.flatMap {
          case Some(tok) => ZIO.some(tok)
          case None =>
            Logger.log.warning("Session token was specified, but is not valid") *>
              HttpResponse.earlyReturn.fromHttpCode.json(HttpCode.`401`)
        }
      case None => ZIO.none
    }

  val userFromSession: HRIO[JDBCConnection & Logger & Telemetry & HttpRequest, M.User.Identity] =
    userFromSessionOptional.someOrElseZIO {
      ZIO.fail(HError.UserError(s"Unauthorized: Specify cookie '$SessionToken'").withHTTPCode(HttpCode.`401`))
    }

  val warnUserPermissions: URIO[Logger, Unit] =
    Logger.log.warning("TODO : User Permissions")

  object convert {

    def user(user: M.User.Identity): D.user.User =
      D.user.User(
        id = user.id.toUUID,
        firstName = user.firstName,
        lastName = user.lastName,
        username = user.username,
        email = user.email,
      )

    def log(log: M.Log.Identity): D.log.Log =
      D.log.Log(
        id = log.id.toUUID,
        appId = log.appId.toUUID,
        logLevel = log.logLevel,
        message = log.message,
        context = log.context,
        dateTime = log.dateTime,
        epochMS = log.epochMS,
        keepUntilEpochMS = log.keepUntilEpochMS,
      )

    def trace(trace: M.Trace.Identity): D.telemetry.Trace =
      D.telemetry.Trace(
        id = trace.id.toUUID,
        appId = trace.appId.toUUID,
        logLevel = trace.logLevel,
        label = trace.label,
        startDateTime = trace.startDateTime,
        endDateTime = trace.endDateTime,
        success = trace.success,
        telemetryContext = trace.telemetryContext,
        logContext = trace.logContext,
        startEpochMS = trace.startEpochMS,
        endEpochMS = trace.endEpochMS,
        keepUntilEpochMS = trace.keepUntilEpochMS,
      )

  }

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

  def appByName(appName: String): HRIO[JDBCConnection & Logger & Telemetry & HttpRequest, M.App.Identity] =
    Q.App.byName(appName).single(s"No such app with name '$appName'")

  def getOrCreateApp(appName: String): HRIO[JDBCConnection & Logger & Telemetry & HttpRequest, M.App.Identity] =
    Q.App.byName(appName).option.flatMap {
      case Some(app) => ZIO.succeed(app)
      case None =>
        val dbApp = makeApp(appName)
        Q.App.insert(dbApp).single.as(dbApp)
    }

  def getOrCreateApps(appNames: Set[String]): HRIO[JDBCConnection & Logger & Telemetry & HttpRequest, Map[String, M.App.Identity]] =
    for {
      existingApps <- Q.App.selectAll().chunk
      existingAppMap = existingApps.map(a => (a.name, a)).toMap

      appsToBeInserted = Chunk.fromIterable(appNames.filterNot(existingAppMap.contains)).map(makeApp)

      _ <- ZIO.when(appsToBeInserted.nonEmpty) { Q.App.insert.batched(appsToBeInserted).unit }
    } yield existingAppMap ++ appsToBeInserted.map(a => (a.name, a)).toMap

  def keepUntilEpochMillis(durationMap: D.app.DurationMap, logLevel: Logger.LogLevel, offsetDateTime: OffsetDateTime): Long =
    offsetDateTime.plus(durationMap.getDuration(logLevel)).toInstant.toEpochMilli

  def keepUntilEpochMillis(durationMap: D.app.DurationMap, logLevel: Option[Logger.LogLevel], offsetDateTime: OffsetDateTime): Long =
    offsetDateTime.plus(durationMap.getDuration(logLevel)).toInstant.toEpochMilli

}
