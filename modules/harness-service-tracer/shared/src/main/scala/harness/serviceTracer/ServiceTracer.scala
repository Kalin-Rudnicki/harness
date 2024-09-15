package harness.serviceTracer

import zio.*

object ServiceTracer {

  // =====| API |=====

  final case class Enabled(enabled: Boolean)

  def trace(closure: TraceClosure, params: (String, String)*): ZIOAspect[Nothing, Any, Nothing, Any, Nothing, Any] =
    new ZIOAspect[Nothing, Any, Nothing, Any, Nothing, Any] {
      def apply[R, E, A](effect: ZIO[R, E, A])(implicit trace: Trace): ZIO[R, E, A] =
        ServiceTracer.runAndTrace(closure, params.toMap, effect)
    }

  def traceWith(closure: TraceClosure, params: (String, String)*)(withTrace: TraceElem => UIO[Unit]): ZIOAspect[Nothing, Any, Nothing, Any, Nothing, Any] =
    new ZIOAspect[Nothing, Any, Nothing, Any, Nothing, Any] {
      def apply[R, E, A](effect: ZIO[R, E, A])(implicit trace: Trace): ZIO[R, E, A] =
        ServiceTracer.runAndTraceWith(closure, params.toMap, effect, withTrace)
    }

  def traceScoped(closure: TraceClosure, params: (String, String)*): URIO[Scope, Unit] =
    runTraceScoped(closure, params.toMap)

  def configure: URIO[ServiceTracer.Enabled, Unit] =
    ZIO.serviceWithZIO[ServiceTracer.Enabled] { enabled =>
      ServiceTracer.runStateRef.update { runState =>
        (enabled.enabled, runState) match {
          case (true, RunState.Disabled) => RunState.Enabled
          case (_, runState)             => runState
        }
      }
    }

  // =====|  |=====

  sealed trait RunState
  object RunState {

    sealed trait NotDisabled extends RunState

    case object Disabled extends RunState // Service Tracing is disable
    case object Enabled extends NotDisabled // Service Tracing is enabled, but nothing would be done with any traces
    case object Running extends NotDisabled // Service Tracing is enabled, and there is something to do with the traces

  }

  private val runStateRef: FiberRef[RunState] =
    Unsafe.unsafely {
      FiberRef.unsafe.make(
        RunState.Disabled,
      )
    }

  private val traceElemsRef: FiberRef[Chunk[TraceElem]] =
    Unsafe.unsafely {
      FiberRef.unsafe.make(
        Chunk.empty[TraceElem],
        _ => Chunk.empty,
        _ ++ _,
      )
    }

  // =====| Helpers |=====

  private def runAndTrace[R, E, A](
      closure: TraceClosure,
      params: Map[String, String],
      effect: ZIO[R, E, A],
  ): ZIO[R, E, A] =
    ServiceTracer.runStateRef.getWith {
      case RunState.Disabled | RunState.Enabled => effect
      case RunState.Running =>
        (for {
          originalTraces <- ServiceTracer.traceElemsRef.getAndSet(Chunk.empty)
          fiberId <- ZIO.fiberId

          start <- Clock.instant
          exit <- effect.interruptible.exit
          end <- Clock.instant

          _ <- ServiceTracer.traceElemsRef.update { children =>
            originalTraces :+ TraceElem(closure, params, fiberId.threadName, start, end, exit.isSuccess, children)
          }

          result <- exit
        } yield result).uninterruptible
    }

  private def runAndTraceWith[R, E, A](
      closure: TraceClosure,
      params: Map[String, String],
      effect: ZIO[R, E, A],
      withTrace: TraceElem => UIO[Unit], // TODO (KR) : support R/E?
  ): ZIO[R, E, A] =
    ServiceTracer.runStateRef.getWith {
      case RunState.Disabled => effect
      case originalRunState: RunState.NotDisabled =>
        (for {
          originalTraces <- ServiceTracer.traceElemsRef.getAndSet(Chunk.empty)
          _ <- ServiceTracer.runStateRef.set(RunState.Running)
          fiberId <- ZIO.fiberId

          start <- Clock.instant
          exit <- effect.interruptible.exit
          end <- Clock.instant

          _ <- ServiceTracer.runStateRef.set(originalRunState)
          newTrace <- ServiceTracer.traceElemsRef.modify { children =>
            val newTrace = TraceElem(closure, params, fiberId.threadName, start, end, exit.isSuccess, children)
            (
              newTrace,
              originalRunState match {
                case RunState.Enabled => originalTraces
                case RunState.Running => originalTraces :+ newTrace
              },
            )
          }
          _ <- withTrace(newTrace)

          result <- exit
        } yield result).uninterruptible
    }

  private def runTraceScoped(
      closure: TraceClosure,
      params: Map[String, String],
  ): URIO[Scope, Unit] =
    ServiceTracer.runStateRef.getWith {
      case RunState.Disabled | RunState.Enabled => ZIO.unit
      case RunState.Running =>
        for {
          fiberId <- ZIO.fiberId
          start <- Clock.instant

          _ <- ZIO.addFinalizerExit { exit =>
            for {
              end <- Clock.instant
              _ <- ServiceTracer.traceElemsRef.update {
                _ :+ TraceElem(closure, params, fiberId.threadName, start, end, exit.isSuccess, Chunk.empty)
              }
            } yield ()
          }
        } yield ()
    }

}
