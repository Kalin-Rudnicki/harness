package template.api

import harness.archive.client.*
import harness.core.*
import harness.docker.*
import harness.docker.sql.DockerPostgres
import harness.email.*
import harness.http.server.*
import harness.payments.service.PaymentProcessor
import harness.sql.*
import harness.sql.autoSchema.*
import harness.sql.query.Transaction
import harness.web.*
import harness.zio.*
import template.api.routes as R
import template.api.service.email.*
import template.api.service.storage.*
import template.model as D
import zio.*
import zio.json.*

object Main extends ExecutableApp {

  override val config: ExecutableApp.Config = ExecutableApp.Config.default.addArchiveDecoders

  type StorageEnv = Transaction & SessionStorage & UserStorage & PaymentMethodStorage
  type ServerEnv = JDBCConnectionPool & EmailService & PaymentProcessor & UiConfig & PaymentProcessor.StripePaymentProcessor.Config
  type ReqEnv = StorageEnv

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
      HConfig.readLayer[PaymentProcessor.StripePaymentProcessor.Config]("payment", "stripe"),
      PaymentProcessor.StripePaymentProcessor.layer,
      HConfig.readLayer[UiConfig]("ui"),
    )

  val storageLayer: URLayer[JDBCConnection, StorageEnv] =
    Transaction.liveLayer ++
      SessionStorage.liveLayer ++
      UserStorage.liveLayer ++
      PaymentMethodStorage.liveLayer

  // This layer will be evaluated for each HTTP request that the server receives
  val reqLayer: SHRLayer[ServerEnv & JDBCConnection & Scope, ReqEnv] =
    storageLayer

  val routes: URIO[RunMode & UiConfig & PaymentProcessor.StripePaymentProcessor.Config & ServerConfig, Route[ServerEnv & ReqEnv]] =
    for {
      runMode <- ZIO.service[RunMode]
      uiConfig <- ZIO.service[UiConfig]
      stripeConfig <- ZIO.service[PaymentProcessor.StripePaymentProcessor.Config]
      config = D.config.UiConfig(
        StdClientConfig(runMode, uiConfig.logTolerance),
        stripeConfig.publishableKey,
      )
      r <- Route.stdRoot(config)(
        R.User.routes,
        R.Payment.routes,
      )
    } yield r

  private val migrations: PlannedMigrations =
    PlannedMigrations(
      Migrations.`0.0.1`,
      Migrations.`0.0.2`,
      Migrations.`0.0.3`,
      Migrations.`0.0.4`,
      Migrations.`0.0.5`,
    )

  override val executable: Executable =
    Executable.fromSubCommands(
      "server" ->
        Executable
          .withLayer[ServerEnv & ServerConfig] {
            serverLayer ++ HConfig.readLayer[ServerConfig]("http")
          }
          .withEffect {
            MigrationRunner.runMigrationsFromPool(migrations) *>
              routes.flatMap { Server.start[ServerEnv, ReqEnv](JDBCConnection.poolLayer >>> reqLayer) }
          },
      "docker" ->
        (DockerPostgres.containerManager).toExecutable {
          DockerNeedsSudo.configLayer("docker", "needsSudo") ++
            DockerAppName.configLayer("docker", "appName") ++
            DbConfig.configLayer ++
            HConfig.readLayer[DockerPostgres.Config]("docker", "postgres")
        },
    )

}
