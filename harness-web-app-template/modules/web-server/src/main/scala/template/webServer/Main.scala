package template.webServer

import harness.email.*
import harness.endpoint.StandardPattern
import harness.endpoint.spec.headerOrCookie
import harness.http.server.*
import harness.payments.service.PaymentProcessor
import harness.sql.*
import harness.sql.autoSchema.*
import harness.web.*
import harness.zio.*
import harness.zio.config.{LoggerConfig, TelemetryConfig}
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

  // TODO (KR) : remove once JWT is used
  final case class SessionTokenHeader(value: String)
  object SessionTokenHeader {
    implicit val jsonDecoder: JsonDecoder[SessionTokenHeader] = JsonDecoder.string.map(SessionTokenHeader(_))
  }

  given JsonDecoder[LoggerConfig] = LoggerConfig.defaultJsonDecoder
  given JsonDecoder[TelemetryConfig] = TelemetryConfig.defaultJsonDecoder

  final case class Config(
      logging: LoggerConfig,
      telemetry: TelemetryConfig,
      db: DbConfig,
      ui: StdClientConfig,
      email: EmailConfig,
      http: Server.Config,
      payment: PaymentConfig,
      sessionTokenHeader: SessionTokenHeader, // TODO (KR) : remove once JWT is used
  ) derives JsonDecoder

  final case class EmailConfig(
      client: EmailClient.LiveOrLoggedConfig,
      service: LiveEmailService.Config,
  ) derives JsonDecoder

  final case class PaymentConfig(
      stripe: PaymentProcessor.StripePaymentProcessor.Config,
  ) derives JsonDecoder

  // type ConfigEnv =

  type ServerEnv =
    Impl.Api.Env & Server.Config

  private val migrations: PlannedMigrations =
    PlannedMigrations(
      Migrations.`0.0.1`,
      Migrations.`0.0.2`,
      Migrations.`0.0.3`,
      Migrations.`0.0.4`,
      Migrations.`0.0.5`,
    )

  val serverLayer: RLayer[Config & Scope, ServerEnv] =
    ZLayer.makeSome[Config, ServerEnv](
      // config
      ZLayer.service[Config].project(_.db),
      ZLayer.service[Config].project(_.email.client),
      ZLayer.service[Config].project(_.email.service),
      ZLayer.service[Config].project(_.http),
      ZLayer.service[Config].project(_.payment.stripe),
      ZLayer.service[Config].project(_.sessionTokenHeader), // TODO (KR) : remove once JWT is used
      ZLayer.fromZIO {
        for {
          http <- ZIO.service[Server.Config]
          token <- ZIO.service[SessionTokenHeader]
        } yield LiveSessionService.Config(http.ssl.nonEmpty, token.value)
      },
      // storage
      Database.poolLayerWithMigrations(migrations),
      Atomically.Live.layer[DomainError],
      LiveUserStorage.liveLayer,
      LivePaymentMethodStorage.liveLayer,
      LiveSessionStorage.liveLayer,
      // service
      EmailClient.liveOrLoggedLayer,
      PaymentProcessor.StripePaymentProcessor.layer,
      LiveSessionService.layer,
      LiveEmailService.liveLayer,
      // api
      UserApi.layer,
      PaymentApi.layer,
    )

  val endpoints: URIO[
    Config,
    StandardPattern[Spec.Api, Endpoint.Projection[Impl.Api.Env]],
  ] =
    for {
      config <- ZIO.service[Config]
      stdUIConfig = config.ui
      serverConfig = config.http
      stripeConfig = config.payment.stripe
      sessionTokenHeader = config.sessionTokenHeader

      uiConfig = Api.config.UiConfig(
        stdUIConfig,
        stripeConfig.publishableKey,
      )

      endpoints = Endpoint.make(
        StandardPattern.spec(Spec.Api.spec(headerOrCookie.raw[Api.user.UserToken](sessionTokenHeader.value))),
        StandardPattern.impl(serverConfig, uiConfig)(Impl.Api.impl),
      )
    } yield endpoints

  override val executable: Executable =
    Executable.fromSubCommands(
      "server" ->
        Executable
          .withConfig[Config]
          .withConfigLogger(_.logging)
          .withConfigTelemetry(_.telemetry)
          .withEnv[Config & ServerEnv] { (config, _) => ZLayer.succeed(config) >+> serverLayer }
          .implement {
            endpoints.flatMap { Server.start[ServerEnv, Any, StandardPattern.Projection[Spec.Api], Impl.Api.Env](ZLayer.empty, _) }
          },
    )

}
