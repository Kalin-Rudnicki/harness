package template.webServer

import harness.email.*
import harness.endpoint.StandardPattern
import harness.endpoint.spec.headerOrCookie
import harness.http.server.*
import harness.payments.service.PaymentProcessor
import harness.sql.*
import harness.sql.autoSchema.*
import harness.sql.query.Transaction
import harness.web.*
import harness.zio.*
import template.api.impl as Impl
import template.api.model as Api
import template.api.service.*
import template.api.spec as Spec
import template.domain.email.*
import template.domain.impl.email.*
import template.domain.impl.session.*
import template.domain.impl.storage.postgres.*
import template.domain.impl.storage.postgres.StorageUtils.errorMapper
import template.domain.model.DomainError
import template.domain.session.*
import zio.*
import zio.json.*

object Main extends ExecutableApp {

  // TODO (KR) :
  // override val config: ExecutableApp.Config = ExecutableApp.Config.default.addArchiveDecoders

  final case class SessionTokenKey(value: String)

  type ServerEnv =
    // Config
    LiveSessionService.Config & Server.Config & PaymentProcessor.StripePaymentProcessor.Config & SessionTokenKey & StdClientConfig &
      // Services
      EmailService & PaymentProcessor &
      // Other
      JDBCConnectionPool
  type ReqEnv =
    // Apis
    UserApi & PaymentApi &
      // Other
      SessionService & Transaction[DomainError]

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
      HConfig.readLayer[Server.Config]("http"),
      HConfig.readLayer[String]("http", "session", "token").project(SessionTokenKey(_)),
      ZLayer.fromZIO {
        for {
          http <- ZIO.service[Server.Config]
          token <- ZIO.service[SessionTokenKey]
        } yield LiveSessionService.Config(http.ssl.nonEmpty, token.value)
      },
    )

  val reqLayer: URLayer[ServerEnv & JDBCConnection & Scope, ReqEnv] =
    ZLayer.makeSome[ServerEnv & JDBCConnection & Scope, ReqEnv](
      // db
      Transaction.liveLayer[DomainError],
      LiveUserStorage.liveLayer,
      LiveSessionStorage.liveLayer,
      LivePaymentMethodStorage.liveLayer,
      // session
      LiveSessionService.layer,
      // api
      UserApi.layer,
      PaymentApi.layer,
    )

  val endpoints: URIO[
    StdClientConfig & PaymentProcessor.StripePaymentProcessor.Config & Server.Config & SessionTokenKey,
    StandardPattern[Spec.Api, Endpoint.Projection[Impl.Api.Env]],
  ] =
    for {
      stdUIConfig <- ZIO.service[StdClientConfig]
      serverConfig <- ZIO.service[Server.Config]
      stripeConfig <- ZIO.service[PaymentProcessor.StripePaymentProcessor.Config]
      sessionTokenKey <- ZIO.service[SessionTokenKey]

      uiConfig = Api.config.UiConfig(
        stdUIConfig,
        stripeConfig.publishableKey,
      )

      endpoints = Endpoint.make(
        StandardPattern.spec(Spec.Api.spec(headerOrCookie.raw[Api.user.UserToken](sessionTokenKey.value))),
        StandardPattern.impl(serverConfig, uiConfig)(Impl.Api.impl),
      )
    } yield endpoints

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
          .withLayer[ServerEnv, Throwable] { serverLayer }
          .withThrowableEffect {
            MigrationRunner.runMigrationsFromPool(migrations) *>
              endpoints.flatMap { Server.start[ServerEnv, ReqEnv, StandardPattern.Projection[Spec.Api], Impl.Api.Env](JDBCConnection.poolLayer >>> reqLayer, _) }
          },
    )

}
