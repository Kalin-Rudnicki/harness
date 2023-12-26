package harness.zio.test

import harness.core.*
import harness.zio.*
import zio.*
import zio.test.*

// NOTE : Any modifications to this file should be reflected in its sister file in harness-zio-test
abstract class HarnessSpec[R: EnvironmentTag] extends ZIOSpec[HarnessEnv & R] {

  final type TestSpec = Spec[Environment & TestEnvironment & Scope, Any]

  // =====| Abstract |=====

  override def spec: TestSpec

  val rLayer: HRLayer[HarnessEnv & Scope, R]

  // =====| Overridable |=====

  lazy val logLevel: Logger.LogLevel = Logger.LogLevel.Warning

  lazy val harnessEnv: HRLayer[Scope, HarnessEnv] =
    HarnessEnv.defaultLayer(logLevel) ++ ZLayer.succeed(RunMode.Dev)

  override def aspects: Chunk[TestAspectAtLeastR[Environment with TestEnvironment]] =
    Chunk(TestAspect.samples(15), TestAspect.shrinks(0), HarnessSpec.logTestPathAndDuration)

  // =====| Concrete |=====

  override final def bootstrap: ZLayer[Scope, Any, Environment] =
    harnessEnv >+> rLayer

}
object HarnessSpec {

  final val logTestPathAndDuration: TestAspectAtLeastR[Logger & Telemetry] =
    new TestAspectAtLeastR[Logger & Telemetry] {

      private def modifySpec[R <: Logger & Telemetry, E](rPath: List[String], spec: Spec[R, E]): Spec[R, E] =
        Spec {
          spec.caseValue match {
            case Spec.ExecCase(exec, spec)     => Spec.ExecCase(exec, modifySpec(rPath, spec))
            case Spec.LabeledCase(label, spec) => Spec.LabeledCase(label, modifySpec(label :: rPath, spec))
            case Spec.ScopedCase(scoped)       => Spec.ScopedCase(scoped.map(modifySpec(rPath, _)))
            case Spec.MultipleCase(specs)      => Spec.MultipleCase(specs.map(modifySpec(rPath, _)))
            case Spec.TestCase(test, annotations) =>
              Spec.TestCase(
                ZIO.clock.flatMap { clock =>
                  Logger.addContext("test-path" -> rPath.reverse.map(n => s"\"$n\"").mkString("[", "/", "]")) {
                    test.withClock(clock).telemetrize("Test Duration").withClock(Clock.ClockLive)
                  }
                },
                annotations,
              )
          }
        }

      override def some[R <: Logger & Telemetry, E](spec: Spec[R, E])(implicit trace: Trace): Spec[R, E] =
        modifySpec(Nil, spec)

    }

}
