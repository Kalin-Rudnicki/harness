package harness.zio.test

import cats.syntax.option.*
import harness.core.*
import harness.zio.*
import zio.*
import zio.test.*

extension (aspect: ZIOAspectPoly) {
  def toTestAspectPoly: TestAspectPoly =
    new TestAspect.PerTest.Poly {
      override def perTest[R, E](test: ZIO[R, TestFailure[E], TestSuccess])(implicit trace: Trace): ZIO[R, TestFailure[E], TestSuccess] =
        test @@ aspect
    }
}

extension [R](aspect: ZIOAspectAtLeastR[R]) {
  def toTestAspectAtLeastR: TestAspectAtLeastR[R] =
    new TestAspect.PerTest.AtLeastR[R] {
      override def perTest[R2 <: R, E](test: ZIO[R2, TestFailure[E], TestSuccess])(implicit trace: Trace): ZIO[R2, TestFailure[E], TestSuccess] =
        test @@ aspect
    }
}

extension (mod: FiberRefModification) {
  def testAspect: TestAspectPoly =
    mod.aspect.toTestAspectPoly
}

extension [R](mod: FiberRefModificationR[R]) {
  def testAspect: TestAspectAtLeastR[R] =
    mod.aspect.toTestAspectAtLeastR
}

object HAspects {

  private[test] val setLoggerSources: TestAspectAtLeastR[LogCache] =
    new TestAspect.PerTest.AtLeastR[LogCache] {

      override def perTest[R <: LogCache, E](test: ZIO[R, TestFailure[E], TestSuccess])(implicit trace: Trace): ZIO[R, TestFailure[E], TestSuccess] =
        ZIO.serviceWithZIO[LogCache] { logCache =>
          test @@
            Logger
              .withSources(
                Logger.Source.const("logCache-trace", LogCache.LoggerTarget(logCache, _.addTrace(_)), Logger.LogLevel.Trace.some),
                Logger.Source.const("logCache-default", LogCache.LoggerTarget(logCache, _.addDefault(_)), None),
                Logger.Source.stdOut(None, ColorMode.Extended, false, true, true),
              )
              .aspect
        }

    }

  private[test] val logTestPathAndDuration: TestAspectPoly =
    new TestAspectPoly {

      private def modifySpec[R, E](rPath: List[String], spec: Spec[R, E]): Spec[R, E] =
        Spec {
          spec.caseValue match {
            case Spec.ExecCase(exec, spec)     => Spec.ExecCase(exec, modifySpec(rPath, spec))
            case Spec.LabeledCase(label, spec) => Spec.LabeledCase(label, modifySpec(label :: rPath, spec))
            case Spec.ScopedCase(scoped)       => Spec.ScopedCase(scoped.map(modifySpec(rPath, _)))
            case Spec.MultipleCase(specs)      => Spec.MultipleCase(specs.map(modifySpec(rPath, _)))
            case Spec.TestCase(test, annotations) =>
              Spec.TestCase(
                ZIO.clock.flatMap { clock =>
                  test.withClock(clock).telemetrize("Test Duration").withClock(Clock.ClockLive) @@
                    Logger.addContext("test-path" -> rPath.reverse.map(n => s"\"$n\"").mkString("[", "/", "]")).aspect
                },
                annotations,
              )
          }
        }

      override def some[R, E](spec: Spec[R, E])(implicit trace: Trace): Spec[R, E] =
        modifySpec(Nil, spec)

    }

}
