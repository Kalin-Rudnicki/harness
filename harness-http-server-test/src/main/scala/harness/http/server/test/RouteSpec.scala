package harness.http.server.test

import harness.core.HError
import harness.http.server.*
import harness.sql.JDBCConnection
import harness.sql.query.*
import harness.zio.*
import scala.util.NotGiven
import zio.*
import zio.json.JsonDecoder
import zio.test.*

abstract class RouteSpec[
    SE <: Transaction: EnvironmentTag,
    RE <: JDBCConnection: EnvironmentTag,
    RE_NC: EnvironmentTag,
](implicit
    ev1: (RE_NC & JDBCConnection) <:< RE,
    ev2: NotGiven[RE_NC <:< JDBCConnection],
) extends ZIOSpec[HarnessEnv & Route[SE & RE] & TestEnvironment] {

  final type ServerEnv = SE
  final type ReqEnv = RE
  final type ReqEnv_NoConnection = RE_NC
  final type ProvidedEnv = Route[ServerEnv & ReqEnv] & CookieStorage
  final type HttpEnv = HarnessEnv & ProvidedEnv & ServerEnv & JDBCConnection

  final type TestSpec = Spec[Environment & ServerEnv & ReqEnv & CookieStorage & Scope, Any]

  // This layer will be evaluated once when the server starts
  val serverLayer: SHRLayer[Scope, ServerEnv]

  // This layer will be evaluated for each test
  val reqLayer: SHRLayer[ServerEnv & Scope, ReqEnv]

  // This layer will be evaluated for each HTTP request that the server receives
  val reqLayerNoConnection: SHRLayer[ServerEnv & JDBCConnection & Scope, ReqEnv_NoConnection]

  val route: ServerConfig => Route[ServerEnv & ReqEnv]
  private final lazy val evalRoute: Route[ServerEnv & ReqEnv] =
    route(ServerConfig(None, "res", true, None))

  lazy val logLevel: Logger.LogLevel =
    Logger.LogLevel.Warning

  final val harnessLayer: HTaskLayer[HarnessEnv] =
    HarnessEnv.defaultLayer(logLevel)

  override def bootstrap: ZLayer[Scope, Any, Environment] =
    harnessLayer ++
      ZLayer.succeed(evalRoute) ++
      testEnvironment

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

  def routeSpec: TestSpec

  override final def spec: Spec[Environment & Scope, Any] =
    (routeSpec @@ runInTransaction)
      .provideSomeLayer[Environment & ServerEnv & Scope] { CookieStorage.emptyLayer ++ reqLayer }
      .provideSomeLayerShared[Environment & Scope] { serverLayer }

  // =====|  |=====

  object httpRequest {

    def apply(request: HttpRequest): HRIO[HttpEnv, HttpResponse.Found] =
      for {
        route <- ZIO.service[Route[ServerEnv & ReqEnv]]
        request <- CookieStorage.applyCookies(request)
        con <- ZIO.service[JDBCConnection]
        layer =
          (ZLayer.succeed(con) >+> reqLayerNoConnection).asInstanceOf[SHRLayer[ServerEnv & Scope, ReqEnv]] ++
            ZLayer.succeed(request) ++
            ZLayer.succeed[Transaction](Transaction.UseSavepointForTransaction)
        response <- ZIO.scoped(route(request.method, request.path).provideSomeLayer(layer))
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

}
