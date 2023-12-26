package harness.http.server.test

import harness.core.HError
import harness.http.server.*
import harness.sql.{JDBCConnection, JDBCConnectionPool}
import harness.sql.query.*
import harness.zio.*
import scala.util.NotGiven
import zio.*
import zio.json.JsonDecoder
import zio.test.*

abstract class RouteSpec[
    SE <: JDBCConnectionPool: EnvironmentTag,
    RE <: Transaction: EnvironmentTag,
] extends ZIOSpec[HarnessEnv & Route[SE & RE] & TestEnvironment] {

  // =====| Types |=====

  final type ServerEnv = SE
  final type ReqEnv = RE
  final type ProvidedEnv = Route[ServerEnv & ReqEnv] & CookieStorage
  final type HttpEnv = HarnessEnv & ProvidedEnv & ServerEnv & JDBCConnection

  final type TestSpec = Spec[Environment & ServerEnv & JDBCConnection & CookieStorage & Scope, Any]

  // =====| Abstract |=====

  // This layer will be evaluated once when the server starts
  val serverLayer: SHRLayer[Scope, ServerEnv]

  // This layer will be evaluated for each http call
  val reqLayer: SHRLayer[ServerEnv & JDBCConnection & Scope, ReqEnv]

  val route: URIO[ServerConfig, Route[ServerEnv & ReqEnv]]

  def routeSpec: TestSpec

  // =====| Overridable |=====

  lazy val logLevel: Logger.LogLevel =
    Logger.LogLevel.Warning

  // =====| API |=====

  object httpRequest {

    def apply(request: HttpRequest): HRIO[HttpEnv, HttpResponse.Found] =
      for {
        route <- ZIO.service[Route[ServerEnv & ReqEnv]]
        request <- CookieStorage.applyCookies(request)
        response <- ZIO.scoped(
          route(request.method, request.path)
            .provideSomeLayer[HttpEnv & Scope](
              reqLayer ++ Transaction.savepointLayer ++ ZLayer.succeed(request),
            ),
        )
        response <- response match {
          case HttpResponse.NotFound        => ZIO.fail(HError.UserError("404 - Not Found"))
          case response: HttpResponse.Found => CookieStorage.update(response.cookies).as(response)
        }
      } yield response

    def stringResponse(request: HttpRequest): HRIO[HttpEnv, String] =
      for {
        response <- httpRequest(request)
        is = java.io.PipedInputStream()
        os = java.io.PipedOutputStream(is)
        len <- HttpResponse.Return.`return`(
          response.`return`,
          os,
          _ => ZIO.unit,
        )
        _ <- ZIO.when(len > Int.MaxValue) {
          ZIO.fail(HError.???("handling response string larger than max int"))
        }
        string <- ZIO.hAttempt(String(is.readNBytes(len.toInt)))
      } yield string

    def jsonResponse[A: JsonDecoder](request: HttpRequest): HRIO[HttpEnv, A] =
      httpRequest.stringResponse(request).flatMap {
        JsonDecoder[A].decodeJson(_) match {
          case Right(a)    => ZIO.succeed(a)
          case Left(error) => ZIO.fail(HError.InternalDefect(s"Unexpected HttpResponse : $error"))
        }
      }

  }

  // =====| Implement |=====

  override def bootstrap: HTaskLayer[Environment] =
    Scope.default >>> (
      harnessLayer ++
        ZLayer.fromZIO(evalRoute) ++
        testEnvironment
    )

  override final def spec: Spec[Environment & Scope, Any] =
    (routeSpec @@ runInTransaction)
      .provideSomeLayer[Environment & ServerEnv & Scope] {
        CookieStorage.emptyLayer ++ JDBCConnection.poolLayer
      }
      .provideSomeLayerShared[Environment & Scope] { serverLayer }

  // =====| Helpers |=====

  private final lazy val evalRoute: UIO[Route[ServerEnv & ReqEnv]] =
    route.provide(ZLayer.succeed(ServerConfig(None, "res", true, None)))

  final val harnessLayer: HTaskLayer[HarnessEnv] =
    HarnessEnv.defaultLayer(logLevel)

  final val runInTransaction: TestAspectAtLeastR[JDBCConnection & Logger & Telemetry] =
    new TestAspectAtLeastR[JDBCConnection & Logger & Telemetry] {

      private def modifySpec[R <: JDBCConnection & Logger & Telemetry, E](rPath: List[String], spec: Spec[R, E]): Spec[R, E] =
        Spec {
          spec.caseValue match {
            case Spec.ExecCase(exec, spec)     => Spec.ExecCase(exec, modifySpec(rPath, spec))
            case Spec.LabeledCase(label, spec) => Spec.LabeledCase(label, modifySpec(label :: rPath, spec))
            case Spec.ScopedCase(scoped)       => Spec.ScopedCase(scoped.map(modifySpec(rPath, _)))
            case Spec.MultipleCase(specs)      => Spec.MultipleCase(specs.map(modifySpec(rPath, _)))
            case Spec.TestCase(test, annotations) =>
              Spec.TestCase(
                Logger.addContext("test-path" -> rPath.reverse.map(n => s"\"$n\"").mkString("[", "/", "]")) {
                  ZIO
                    .acquireReleaseWith { Transaction.raw.begin().unit.orDie }
                    .apply { _ => Transaction.raw.rollback().unit.orDie }
                    .apply { _ => test }
                },
                annotations,
              )
          }
        }

      override def some[R <: JDBCConnection & Logger & Telemetry, E](spec: Spec[R, E])(implicit trace: Trace): Spec[R, E] =
        modifySpec(Nil, spec)

    }

}
