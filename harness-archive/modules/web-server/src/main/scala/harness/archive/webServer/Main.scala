package harness.archive.webServer

import harness.email.*
import harness.http.server.*
import harness.http.server.ServerConfig
import harness.payments.service.PaymentProcessor
import harness.sql.*
import harness.sql.autoSchema.*
import harness.sql.query.Transaction
import harness.web.*
import harness.zio.*
import harness.archive.api.model as Api
import harness.archive.domain.email.*
import harness.archive.domain.impl.email.*
import harness.archive.domain.impl.storage.postgres.*
import harness.archive.webServer.api.*
import harness.archive.webServer.route.*
import zio.*
import zio.json.*

object Main extends ExecutableApp {

  // TODO (KR) :
  // override val config: ExecutableApp.Config = ExecutableApp.Config.default.addArchiveDecoders

  type ServerEnv = JDBCConnectionPool & EmailService & PaymentProcessor & StdClientConfig & PaymentProcessor.StripePaymentProcessor.Config & SessionConfig
  type ReqEnv = UserApi & PaymentApi & Transaction

  val serverLayer: RLayer[HarnessEnv & Scope, ServerEnv] =
    ZLayer.makeSome[HarnessEnv & Scope, ServerEnv](
      HConfig.readLayer[DbConfig]("db"),
      JDBCConnectionPool.configLayer,
      HConfig.readLayer[EmailConfig]("email", "client"),
      EmailClient.liveLayer,
      HConfig.readLayer[Boolean]("email", "service", "live").flatMap { live =>
        if (live.get) HConfig.readLayer[LiveEmailService.Config]("email", "service") >>> LiveEmailService.liveLayer
        else LiveEmailService.logLayer
      },
      HConfig.readLayer[PaymentProcessor.StripePaymentProcessor.Config]("payment", "stripe"),
      PaymentProcessor.StripePaymentProcessor.layer,
      HConfig.readLayer[StdClientConfig]("ui"),
      HConfig.readLayer[SessionConfig]("http", "session"),
    )

  val reqLayer: URLayer[ServerEnv & JDBCConnection & Scope, ReqEnv] =
    ZLayer.makeSome[ServerEnv & JDBCConnection & Scope, ReqEnv](
      // db
      Transaction.liveLayer,
      LiveUserStorage.liveLayer,
      LiveSessionStorage.liveLayer,
      LivePaymentMethodStorage.liveLayer,
      // api
      UserApi.layer,
      PaymentApi.layer,
    )

  val routes: URIO[StdClientConfig & PaymentProcessor.StripePaymentProcessor.Config & ServerConfig, Route[ServerEnv & ReqEnv]] =
    for {
      uiConfig <- ZIO.service[StdClientConfig]
      stripeConfig <- ZIO.service[PaymentProcessor.StripePaymentProcessor.Config]
      config = Api.config.UiConfig(
        uiConfig,
        stripeConfig.publishableKey,
      )
      r <- Route.stdRoot(config)(
        UserRoutes.routes,
        PaymentRoutes.routes,
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
    Executable
      .withLayer[ServerEnv & ServerConfig, Throwable] {
        serverLayer ++ HConfig.readLayer[ServerConfig]("http")
      }
      .withEffectSimple {
        MigrationRunner.runMigrationsFromPool(migrations) *>
          routes.flatMap { Server.start[ServerEnv, ReqEnv](JDBCConnection.poolLayer >>> reqLayer) }
      }

}
