package harness.zio.test

import harness.zio.*
import zio.*
import zio.{Trace, ZIO}
import zio.test.*

object HAspects {

  object logger {

    def withLevel(logLevel: Logger.LogLevel): TestAspectAtLeastR[Logger] =
      new TestAspect.PerTest.AtLeastR[Logger] {

        override def perTest[R <: Logger, E](
            test: ZIO[R, TestFailure[E], TestSuccess],
        )(implicit trace: Trace): ZIO[R, TestFailure[E], TestSuccess] =
          test.updateService[Logger] { _.copy(defaultMinLogTolerance = logLevel) }

      }

    def withContext(pairs: (String, Any)*): TestAspectAtLeastR[Logger] =
      new TestAspect.PerTest.AtLeastR[Logger] {

        override def perTest[R <: Logger, E](
            test: ZIO[R, TestFailure[E], TestSuccess],
        )(implicit trace: Trace): ZIO[R, TestFailure[E], TestSuccess] =
          test.updateService[Logger] { _.addContext(pairs.map { case (k, v) => (k, v.toString) }.toMap) }

      }

  }

  private[test] val logTestPathAndDuration: TestAspectAtLeastR[Logger & Telemetry] =
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
