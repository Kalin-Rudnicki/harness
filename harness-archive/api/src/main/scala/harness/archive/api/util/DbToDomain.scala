package harness.archive.api.util

import harness.archive.api.db.model as M
import harness.archive.model as D

object DbToDomain {

  def user(user: M.User.Identity): D.user.User =
    D.user.User(
      id = user.id,
      firstName = user.firstName,
      lastName = user.lastName,
      username = user.username,
      email = user.email,
    )

  def log(log: M.Log.Identity): D.log.Log =
    D.log.Log(
      id = log.id,
      appId = log.appId,
      logLevel = log.logLevel,
      message = log.message,
      context = log.context,
      dateTime = log.dateTime,
      epochMS = log.epochMS,
      keepUntilEpochMS = log.keepUntilEpochMS,
    )

  def trace(trace: M.Trace.Identity): D.telemetry.Trace =
    D.telemetry.Trace(
      id = trace.id,
      appId = trace.appId,
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

  def app(app: M.App.Identity): D.app.App =
    D.app.App(
      id = app.id,
      name = app.name,
      logDurationMap = app.logDurationMap,
      traceDurationMap = app.traceDurationMap,
    )

}
