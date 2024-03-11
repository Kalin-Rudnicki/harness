package harness.archive.db.model

import harness.archive
import harness.archive.api.model as Api
import harness.archive.domain.model as Domain
import harness.email.EmailAddress
import harness.sql.*
import harness.zio.Logger
import java.time.OffsetDateTime

final case class User[F[_]](
    id: F[User.Id],
    firstName: F[String],
    lastName: F[String],
    username: F[String],
    lowerUsername: F[String],
    encryptedPassword: F[String],
    email: F[EmailAddress],
) extends Table.WithId[F, User.Id]
object User extends Table.Companion.WithId[Api.user.UserId, User] {

  def fromDomain(user: Domain.User): User.Identity =
    new User.Identity(
      id = user.id,
      firstName = user.firstName,
      lastName = user.lastName,
      username = user.username,
      lowerUsername = user.lowerUsername,
      encryptedPassword = user.encryptedPassword,
      email = user.email,
    )

  def toDomain(user: User.Identity): Domain.User =
    Domain.User(
      id = user.id,
      firstName = user.firstName,
      lastName = user.lastName,
      username = user.username,
      lowerUsername = user.lowerUsername,
      encryptedPassword = user.encryptedPassword,
      email = user.email,
    )

  override implicit lazy val tableSchema: TableSchema[User] =
    TableSchema.derived[User]("user_data", "user") {
      new User.Cols(
        id = User.Id.pkCol,
        firstName = Col.string("first_name"),
        lastName = Col.string("last_name"),
        username = Col.string("username"),
        lowerUsername = Col.string("lower_username"),
        encryptedPassword = Col.string("encrypted_password"),
        email = Col.encoded[EmailAddress]("email"),
      )
    }

}

final case class Session[F[_]](
    id: F[Session.Id],
    userId: F[User.Id],
    token: F[Api.user.UserToken],
) extends Table.WithId[F, Session.Id]
object Session extends Table.Companion.WithId[Api.user.SessionId, Session] {

  def fromDomain(session: Domain.Session): Session.Identity =
    new Session.Identity(
      id = session.id,
      userId = session.userId,
      token = session.token,
    )

  def toDomain(session: Session.Identity): Domain.Session =
    Domain.Session(
      id = session.id,
      userId = session.userId,
      token = session.token,
    )

  override implicit lazy val tableSchema: TableSchema[Session] =
    TableSchema.derived[Session]("user_data", "session") {
      new Session.Cols(
        id = Session.Id.pkCol,
        userId = User.Id.fkCol("user_id"),
        token = Col.encoded[Api.user.UserToken]("token"),
      )
    }

}

final case class App[F[_]](
    id: F[App.Id],
    userId: F[User.Id],
    name: F[String],
    logDurationMap: F[Api.app.DurationMap],
    traceDurationMap: F[Api.app.DurationMap],
) extends Table.WithId[F, App.Id]
object App extends Table.Companion.WithId[Api.app.AppId, App] {

  def fromDomain(app: Domain.App): App.Identity =
    new App.Identity(
      id = app.id,
      userId = app.userId,
      name = app.name,
      logDurationMap = app.logDurationMap,
      traceDurationMap = app.traceDurationMap,
    )

  def toDomain(app: App.Identity): Domain.App =
    Domain.App(
      id = app.id,
      userId = app.userId,
      name = app.name,
      logDurationMap = app.logDurationMap,
      traceDurationMap = app.traceDurationMap,
    )

  override implicit lazy val tableSchema: TableSchema[App] =
    TableSchema.derived[App]("archive", "app") {
      new App.Cols(
        id = App.Id.pkCol,
        userId = User.Id.fkCol,
        name = Col.string("name"),
        logDurationMap = Col.jsonb[Api.app.DurationMap]("log_duration_map"),
        traceDurationMap = Col.jsonb[Api.app.DurationMap]("trace_duration_map"),
      )
    }

}

final case class AppToken[F[_]](
    id: F[AppToken.Id],
    appId: F[App.Id],
    name: F[String],
    createdAt: F[OffsetDateTime],
    token: F[Api.app.AppToken],
) extends Table.WithId[F, AppToken.Id]
object AppToken extends Table.Companion.WithId[Api.app.AppTokenId, AppToken] {

  def fromDomain(appToken: Domain.AppToken): AppToken.Identity =
    new AppToken.Identity(
      id = appToken.id,
      appId = appToken.appId,
      name = appToken.name,
      createdAt = appToken.createdAt,
      token = appToken.token,
    )

  def toDomain(appToken: AppToken.Identity): Domain.AppToken =
    Domain.AppToken(
      id = appToken.id,
      appId = appToken.appId,
      name = appToken.name,
      createdAt = appToken.createdAt,
      token = appToken.token,
    )

  override implicit lazy val tableSchema: TableSchema[AppToken] =
    TableSchema.derived[AppToken]("app_token") {
      new AppToken.Cols(
        id = AppToken.Id.pkCol,
        appId = App.Id.fkCol,
        name = Col.string("name"),
        createdAt = Col.offsetDateTime("created_at"),
        token = Col.encoded[Api.app.AppToken]("token"),
      )
    }

}

final case class Log[F[_]](
    id: F[Log.Id],
    appId: F[App.Id],
    logLevel: F[Option[Logger.LogLevel]],
    message: F[String],
    context: F[Map[String, String]],
    dateTime: F[OffsetDateTime],
    epochMS: F[Long],
    keepUntilEpochMS: F[Long],
) extends Table.WithId[F, Log.Id]
object Log extends Table.Companion.WithId[Api.log.LogId, Log] {

  def fromDomain(log: Domain.Log): Log.Identity =
    new Log.Identity(
      id = log.id,
      appId = log.appId,
      logLevel = log.logLevel,
      message = log.message,
      context = log.context,
      dateTime = log.dateTime,
      epochMS = log.epochMS,
      keepUntilEpochMS = log.keepUntilEpochMS,
    )

  def toDomain(log: Log.Identity): Domain.Log =
    Domain.Log(
      id = log.id,
      appId = log.appId,
      logLevel = log.logLevel,
      message = log.message,
      context = log.context,
      dateTime = log.dateTime,
      epochMS = log.epochMS,
      keepUntilEpochMS = log.keepUntilEpochMS,
    )

  override implicit lazy val tableSchema: TableSchema[Log] =
    TableSchema.derived[Log]("archive", "log") {
      new Log.Cols(
        id = Log.Id.pkCol,
        appId = App.Id.fkCol("app_id"),
        logLevel = Col.string("log_level").iemap(Logger.LogLevel.stringDecoder.decodeAccumulating)(Logger.LogLevel.stringEncoder.encode).optional,
        message = Col.string("message"),
        context = Col.jsonb("context"),
        dateTime = Col.offsetDateTime("date_time"),
        epochMS = Col.long("epoch_ms"),
        keepUntilEpochMS = Col.long("keep_until_epoch_ms"),
      )
    }

}

final case class Trace[F[_]](
    id: F[Trace.Id],
    appId: F[App.Id],
    logLevel: F[Logger.LogLevel],
    label: F[String],
    startDateTime: F[OffsetDateTime],
    endDateTime: F[OffsetDateTime],
    success: F[Boolean],
    telemetryContext: F[Map[String, String]],
    logContext: F[Map[String, String]],
    startEpochMS: F[Long],
    endEpochMS: F[Long],
    keepUntilEpochMS: F[Long],
) extends Table.WithId[F, Trace.Id]
object Trace extends Table.Companion.WithId[Api.telemetry.TraceId, Trace] {

  def fromDomain(trace: Domain.Trace): Trace.Identity =
    new Trace.Identity(
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

  def toDomain(trace: Trace.Identity): Domain.Trace =
    Domain.Trace(
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

  override implicit lazy val tableSchema: TableSchema[Trace] =
    TableSchema.derived[Trace]("archive", "trace") {
      new Trace.Cols(
        id = Trace.Id.pkCol,
        appId = App.Id.fkCol("app_id"),
        logLevel = Col.encoded[Logger.LogLevel]("log_level"),
        label = Col.string("label"),
        startDateTime = Col.offsetDateTime("start_date_time"),
        endDateTime = Col.offsetDateTime("end_date_time"),
        success = Col.boolean("success"),
        telemetryContext = Col.jsonb("telemetry_context"),
        logContext = Col.jsonb("log_context"),
        startEpochMS = Col.long("start_epoch_ms"),
        endEpochMS = Col.long("end_epoch_ms"),
        keepUntilEpochMS = Col.long("keep_until_epoch_ms"),
      )
    }

}
