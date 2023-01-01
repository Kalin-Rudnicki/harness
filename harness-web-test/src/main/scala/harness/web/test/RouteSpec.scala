package harness.web.test

import harness.core.HError
import harness.sql.JDBCConnection
import harness.sql.query.*
import harness.web.server.*
import harness.zio.*
import zio.*
import zio.json.JsonDecoder
import zio.test.*

abstract class RouteSpec[SE <: Transaction: EnvironmentTag, RE <: JDBCConnection: EnvironmentTag] extends ZIOSpec[HarnessEnv & Route[SE & RE] & TestEnvironment] {

  final type ServerEnv = SE
  final type ReqEnv = RE
  final type ProvidedEnv = Route[ServerEnv & ReqEnv] & CookieStorage
  final type HttpEnv = HarnessEnv & ProvidedEnv & ServerEnv & JDBCConnection

  final type TestSpec = Spec[Environment & ServerEnv & ReqEnv & CookieStorage & Scope, Any]

  // This layer will be evaluated once when the server starts
  val serverLayer: SHRLayer[Scope, ServerEnv]

  // This layer will be evaluated for each HTTP request that the server receives
  val reqLayer: SHRLayer[ServerEnv & Scope, ReqEnv]

  val route: ServerConfig => Route[ServerEnv & ReqEnv]
  private final lazy val evalRoute: Route[ServerEnv & ReqEnv] =
    route(ServerConfig(None, None, "res"))

  lazy val logLevel: Logger.LogLevel =
    Logger.LogLevel.Warning

  final val harnessLayer: HTaskLayer[HarnessEnv] =
    HarnessEnv.defaultLayer(logLevel)

  override def bootstrap: ZLayer[Scope, Any, Environment] =
    harnessLayer ++
      ZLayer.succeed(evalRoute) ++
      testEnvironment

  final val runInTransaction: TestAspectAtLeastR[JDBCConnection & Logger] =
    new TestAspectAtLeastR[JDBCConnection & Logger] {

      private def modifySpec[R <: JDBCConnection & Logger, E](rPath: List[String], spec: Spec[R, E]): Spec[R, E] =
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
                    .acquireReleaseWith { Transaction.raw.begin().orDie.unit }
                    .apply { _ => Transaction.raw.rollback().orDie.unit }
                    .apply { _ => test }
                },
                annotations,
              )
          }
        }

      override def some[R <: JDBCConnection & Logger, E](spec: Spec[R, E])(implicit trace: Trace): Spec[R, E] =
        modifySpec(Nil, spec)

    }

  def routeSpec: TestSpec

  override final def spec: Spec[Environment & Scope, Any] =
    (routeSpec @@ runInTransaction).provideSomeLayer[Environment & Scope] {
      CookieStorage.emptyLayer ++
        (serverLayer >+> reqLayer)
    }

  // =====|  |=====

  object httpRequest {

    def apply(request: HttpRequest): HRIO[HttpEnv, HttpResponse.Found] =
      for {
        route <- ZIO.service[Route[ServerEnv & ReqEnv]]
        request <- CookieStorage.applyCookies(request)
        con <- ZIO.service[JDBCConnection]
        layer =
          reqLayer ++
            ZLayer.succeed(con) ++
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
        _ <- ZIO.hAttempt(response.write(os))
        string <- ZIO.hAttempt(String(is.readNBytes(is.available())))
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
