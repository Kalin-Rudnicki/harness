package harness.archive.api.main

import harness.archive.api.routes as R
import harness.archive.api.service.*
import harness.archive.api.service.email.EmailService
import harness.archive.api.service.storage.*
import harness.cli.*
import harness.core.*
import harness.email.*
import harness.http.server.*
import harness.sql.*
import harness.sql.autoSchema.*
import harness.sql.query.Transaction
import harness.web.*
import harness.zio.*
import harness.zio.config.HConfig
import zio.*
import zio.json.*

object ServerMain {

  type StorageEnv = Transaction & SessionStorage & UserStorage & AppStorage & AppTokenStorage & LogStorage & TraceStorage
  type ServerEnv = JDBCConnectionPool & EmailService & StaleDataCleanser & UiConfig
  type ReqEnv = StorageEnv

  private final case class Config(
      startStaleDataCleanser: Boolean,
  )
  private object Config {

    val parser: Parser[Config] =
      (
        Parser
          .flag(
            LongName.unsafe("start-stale-data-cleanser"),
            shortParam = Defaultable.Some(ShortName.unsafe('C')),
            helpHint = List("Run the stale data cleanser in parallel with server"),
          ),
        )
        .map(Config.apply)

  }

  final case class UiConfig(
      logTolerance: Logger.LogLevel,
  )
  object UiConfig {
    implicit val jsonCodec: JsonCodec[UiConfig] = DeriveJsonCodec.gen
  }

  // This layer will be evaluated once when the server starts
  val serverLayer: SHRLayer[Scope, ServerEnv] =
    ZLayer.makeSome[HarnessEnv & Scope, ServerEnv](
      HConfig.readLayer[DbConfig]("db"),
      JDBCConnectionPool.configLayer,
      HConfig.readLayer[EmailConfig]("email", "client"),
      EmailClient.liveLayer,
      HConfig.readLayer[EmailService.Config]("email", "service"),
      EmailService.liveLayer,
      StaleDataCleanser.live(1.minute, 1.minute, 1.minute, 5.minutes, 15.minutes, 1.hour, 1.hour, 6.hours),
      HConfig.readLayer[UiConfig]("ui"),
    )

  val storageLayer: URLayer[JDBCConnection, StorageEnv] =
    Transaction.liveLayer ++
      SessionStorage.liveLayer ++
      UserStorage.liveLayer ++
      AppStorage.liveLayer ++
      AppTokenStorage.liveLayer ++
      LogStorage.liveLayer ++
      TraceStorage.liveLayer

  // This layer will be evaluated for each HTTP request that the server receives
  val reqLayer: SHRLayer[ServerEnv & JDBCConnection & Scope, ReqEnv] =
    storageLayer

  val routes: URIO[RunMode & UiConfig & ServerConfig, Route[ServerEnv & ReqEnv]] =
    for {
      runMode <- ZIO.service[RunMode]
      uiConfig <- ZIO.service[UiConfig]
      config = StdClientConfig(runMode, uiConfig.logTolerance).basic
      r <- Route.stdRoot(config)(
        R.User.routes,
        R.App.routes,
        R.Log.routes,
        R.Telemetry.routes,
      )
    } yield r

  private val migrations: PlannedMigrations =
    PlannedMigrations(
      Migrations.`0.0.1`,
      Migrations.`0.0.2`,
      Migrations.`0.0.3`,
      Migrations.`0.0.4`,
    )

  val executable: Executable =
    Executable
      .withParser(Config.parser)
      .withLayer[ServerEnv & ServerConfig] {
        serverLayer ++ HConfig.readLayer[ServerConfig]("http")
      }
      .withEffect { config =>
        StaleDataCleanser.startFiber.when(config.startStaleDataCleanser) *>
          MigrationRunner.runMigrationsFromPool(migrations) *>
          routes.flatMap { Server.start[ServerEnv, ReqEnv](JDBCConnection.poolLayer >>> reqLayer) }
      }

}
