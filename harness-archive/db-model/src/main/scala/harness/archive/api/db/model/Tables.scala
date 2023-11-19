package harness.archive.api.db.model

import harness.archive
import harness.archive.api
import harness.archive.api.db
import harness.archive.api.db.model
import harness.archive.model as D
import harness.sql.*
import harness.zio.Logger
import java.time.OffsetDateTime
import java.util.UUID

final case class User[F[_]](
    id: F[User.Id],
    firstName: F[String],
    lastName: F[String],
    username: F[String],
    lowerUsername: F[String],
    encryptedPassword: F[String],
    email: F[String],
) extends Table.WithId[F, User.Id] {
  def show: String = s"'$username' ($id)"
}
object User extends Table.Companion.WithId[D.user.UserId, User] {

  override implicit lazy val tableSchema: TableSchema[User] =
    TableSchema.derived[User]("user_data", "user") {
      new User.Cols(
        id = User.Id.pkCol,
        firstName = Col.string("first_name"),
        lastName = Col.string("last_name"),
        username = Col.string("username"),
        lowerUsername = Col.string("lower_username"),
        encryptedPassword = Col.string("encrypted_password"),
        email = Col.string("email"),
      )
    }

}

final case class Session[F[_]](
    id: F[Session.Id],
    userId: F[User.Id],
    token: F[String],
) extends Table.WithId[F, Session.Id]
object Session extends Table.Companion.WithId[D.user.SessionId, Session] {

  override implicit lazy val tableSchema: TableSchema[Session] =
    TableSchema.derived[Session]("user_data", "session") {
      new Session.Cols(
        id = Session.Id.pkCol,
        userId = User.Id.fkCol("user_id"),
        token = Col.string("token"),
      )
    }

  def newForUser(user: User.Identity): Session.Identity =
    new Session.Identity(
      id = Session.Id.gen,
      userId = user.id,
      token = s"${UUID.randomUUID}:${UUID.randomUUID}",
    )

}

final case class App[F[_]](
    id: F[App.Id],
    name: F[String],
    logDurationMap: F[D.app.DurationMap],
    traceDurationMap: F[D.app.DurationMap],
) extends Table.WithId[F, App.Id]
object App extends Table.Companion.WithId[D.app.AppId, App] {

  override implicit lazy val tableSchema: TableSchema[App] =
    TableSchema.derived[App]("archive", "app") {
      new App.Cols(
        id = App.Id.pkCol,
        name = Col.string("name"),
        logDurationMap = Col.jsonb[D.app.DurationMap]("log_duration_map"),
        traceDurationMap = Col.jsonb[D.app.DurationMap]("trace_duration_map"),
      )
    }

}

final case class Log[F[_]](
    id: F[db.model.Log.Id],
    appId: F[App.Id],
    logLevel: F[Option[Logger.LogLevel]],
    message: F[String],
    context: F[Map[String, String]],
    dateTime: F[OffsetDateTime],
    epochMS: F[Long],
    keepUntilEpochMS: F[Long],
) extends Table.WithId[F, db.model.Log.Id]
object Log extends Table.Companion.WithId[D.log.LogId, Log] {

  override implicit lazy val tableSchema: TableSchema[Log] =
    TableSchema.derived[Log]("archive", "log") {
      new db.model.Log.Cols(
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
    id: F[archive.api.db.model.Trace.Id],
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
) extends Table.WithId[F, archive.api.db.model.Trace.Id]
object Trace extends Table.Companion.WithId[D.telemetry.TraceId, Trace] {

  override implicit lazy val tableSchema: TableSchema[Trace] =
    TableSchema.derived[Trace]("archive", "trace") {
      new archive.api.db.model.Trace.Cols(
        id = Trace.Id.pkCol,
        appId = App.Id.fkCol("app_id"),
        logLevel = Col.string("log_level").iemap(Logger.LogLevel.stringDecoder.decodeAccumulating)(Logger.LogLevel.stringEncoder.encode),
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
