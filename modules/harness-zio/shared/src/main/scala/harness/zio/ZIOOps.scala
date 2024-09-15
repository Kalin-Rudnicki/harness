package harness.zio

import cats.data.NonEmptyList
import cats.syntax.list.*
import java.time.OffsetDateTime
import scala.annotation.tailrec
import zio.*

// =====| ZIO._ |=====

implicit class ZIOCompanionOps(self: ZIO.type) {

  def acquireClosable[R, E, A <: java.io.Closeable](acq: => ZIO[R, E, A]): ZIO[R & Scope, E, A] =
    ZIO.acquireRelease(acq)(c => ZIO.attempt(c.close()).orDie)
  def acquireAutoClosable[R, E, A <: java.lang.AutoCloseable](acq: => ZIO[R, E, A]): ZIO[R & Scope, E, A] =
    ZIO.acquireRelease(acq)(c => ZIO.attempt(c.close()).orDie)

  def traverse[A, R, E, B](as: NonEmptyList[A])(f: A => ZIO[R, E, B]): ZIO[R, E, NonEmptyList[B]] =
    ZIO.foreach(as.toList)(f(_).exit).map(NonEmptyList.fromListUnsafe).flatMap { nel =>
      @tailrec
      def fails(queue: List[Exit[E, B]], rStack: NonEmptyList[Cause[E]]): ZIO[R, E, Nothing] =
        queue match {
          case Exit.Success(_) :: tail => fails(tail, rStack)
          case Exit.Failure(e) :: tail => fails(tail, e :: rStack)
          case Nil                     => ZIO.refailCause(rStack.reverse.reduceLeft(Cause.Both(_, _)))
        }

      @tailrec
      def successes(queue: List[Exit[E, B]], rStack: NonEmptyList[B]): ZIO[R, E, NonEmptyList[B]] =
        queue match {
          case Exit.Success(b) :: tail => successes(tail, b :: rStack)
          case Exit.Failure(e) :: tail => fails(tail, NonEmptyList.one(e))
          case Nil                     => ZIO.succeed(rStack.reverse)
        }

      nel.head match {
        case Exit.Success(b) => successes(nel.tail, NonEmptyList.one(b))
        case Exit.Failure(e) => fails(nel.tail, NonEmptyList.one(e))
      }
    }

  def traverse[A, R, E, B](as: List[A])(f: A => ZIO[R, E, B]): ZIO[R, E, List[B]] =
    as.toNel match {
      case Some(nel) => ZIO.traverse(nel)(f).map(_.toList)
      case None      => ZIO.succeed(Nil)
    }

}

// =====| ZIO |=====

implicit class ZIOOps[R, E, A](self: ZIO[R, E, A]) {

  def whenDiscard(p: => Boolean): ZIO[R, E, Unit] =
    self.when(p).unit
  def whenZIODiscard[R2 <: R, E2 >: E](p: => ZIO[R2, E2, Boolean]): ZIO[R2, E2, Unit] =
    self.whenZIO(p).unit

  def unlessDiscard(p: => Boolean): ZIO[R, E, Unit] =
    self.unless(p).unit
  def unlessZIODiscard[R2 <: R, E2 >: E](p: => ZIO[R2, E2, Boolean]): ZIO[R2, E2, Unit] =
    self.unlessZIO(p).unit

  def mapErrorTo[E2](implicit errorMapper: ErrorMapper[E, E2]): ZIO[R, E2, A] =
    self.mapError(errorMapper.mapError)

  def partialStacklessFailures(f: E => Boolean): ZIO[R, E, A] =
    self.mapErrorCause(_.partialStacklessFailures(f))
  def stacklessFailures: ZIO[R, E, A] =
    self.mapErrorCause(_.stacklessFailures)
  def stacklessCause: ZIO[R, E, A] =
    self.mapErrorCause(Cause.stackless)

}

implicit class ZIOAutoClosableOps[R, E, A <: java.lang.AutoCloseable](self: ZIO[R, E, A]) {

  def autoClose: ZIO[R & Scope, E, A] = ZIO.fromAutoCloseable { self }

}

implicit class OptionZIOOps[R, E, A](self: ZIO[R, E, Option[A]]) {

  def someOrElseZIOOpt[R2, E2 >: E, A2 >: A](other: => ZIO[R2, E2, Option[A2]]): ZIO[R & R2, E2, Option[A2]] =
    self.flatMap {
      case some @ Some(_) => ZIO.succeed(some)
      case None           => other
    }

}

// =====| Cause |=====

implicit class CauseOps[E](self: Cause[E]) {

  def causeFailuresOpt: Option[NonEmptyList[Cause.Fail[E]]] =
    self
      .foldLeft(List.empty[Cause.Fail[E]]) { case (z, fail: Cause.Fail[E]) => fail :: z }
      .reverse
      .toNel

  def collapsedCauseFailures(implicit errorMapper: ErrorMapper[Throwable, E]): NonEmptyList[Cause.Fail[E]] =
    self.collapse.causeFailuresOpt match
      case Some(value) => value
      case None        => throw new RuntimeException("collapsedCauseFailures = Nil, should not be possible")

  def collapse(implicit errorMapper: ErrorMapper[Throwable, E]): Cause[E] =
    self match
      case Cause.Empty                       => Cause.fail(errorMapper.mapError(new RuntimeException("Empty Cause")))
      case Cause.Fail(value, trace)          => Cause.Fail(value, trace)
      case Cause.Die(value, trace)           => Cause.Fail(errorMapper.mapError(value), trace)
      case Cause.Interrupt(fiberId, trace)   => Cause.Fail(errorMapper.mapError(new RuntimeException(s"Interrupted by fiber $fiberId")), trace)
      case Cause.Stackless(cause, stackless) => Cause.Stackless(cause.collapse, stackless)
      case Cause.Then(left, right)           => Cause.Then(left.collapse, right.collapse)
      case Cause.Both(left, right)           => Cause.Both(left.collapse, right.collapse)

  def stacklessFailures: Cause[E] = self match
    case fail @ Cause.Fail(_, _) => Cause.stackless(fail)
    case Cause.Then(left, right) => Cause.Then(left.stacklessFailures, right.stacklessFailures)
    case Cause.Both(left, right) => Cause.Both(left.stacklessFailures, right.stacklessFailures)
    case _                       => self

  def partialStacklessFailures(f: E => Boolean): Cause[E] = self match
    case fail @ Cause.Fail(e, _) if f(e) => Cause.stackless(fail)
    case Cause.Then(left, right)         => Cause.Then(left.stacklessFailures, right.stacklessFailures)
    case Cause.Both(left, right)         => Cause.Both(left.stacklessFailures, right.stacklessFailures)
    case _                               => self

}

// =====| Logging / Telemetry |=====

implicit class ZIOLogTelemetryOps[R, E, A](self: ZIO[R, E, A]) {

  object telemetrize {

    def apply(label: String, logLevel: Logger.LogLevel, telemetryContext: (String, Any)*)(implicit trace: Trace): ZIO[R, E, A] =
      self @@ Telemetry.telemetrize(label, logLevel, telemetryContext*)

    def apply(label: String, telemetryContext: (String, Any)*)(implicit trace: Trace): ZIO[R, E, A] =
      apply(label, Logger.LogLevel.Trace, telemetryContext*)

    // Note : WithLogLevelAbstract was not giving good type hints
    def apply(logLevel: Logger.LogLevel)(label: String, telemetryContext: (String, Any)*): ZIO[R, E, A] = apply(label, logLevel, telemetryContext*)
    def never(label: String, telemetryContext: (String, Any)*)(implicit trace: Trace): ZIO[R, E, A] = apply(label, Logger.LogLevel.Never, telemetryContext*)
    def trace(label: String, telemetryContext: (String, Any)*)(implicit trace: Trace): ZIO[R, E, A] = apply(label, Logger.LogLevel.Trace, telemetryContext*)
    def debug(label: String, telemetryContext: (String, Any)*)(implicit trace: Trace): ZIO[R, E, A] = apply(label, Logger.LogLevel.Debug, telemetryContext*)
    def detailed(label: String, telemetryContext: (String, Any)*)(implicit trace: Trace): ZIO[R, E, A] = apply(label, Logger.LogLevel.Detailed, telemetryContext*)
    def info(label: String, telemetryContext: (String, Any)*)(implicit trace: Trace): ZIO[R, E, A] = apply(label, Logger.LogLevel.Info, telemetryContext*)
    def important(label: String, telemetryContext: (String, Any)*)(implicit trace: Trace): ZIO[R, E, A] = apply(label, Logger.LogLevel.Important, telemetryContext*)
    def warning(label: String, telemetryContext: (String, Any)*)(implicit trace: Trace): ZIO[R, E, A] = apply(label, Logger.LogLevel.Warning, telemetryContext*)
    def error(label: String, telemetryContext: (String, Any)*)(implicit trace: Trace): ZIO[R, E, A] = apply(label, Logger.LogLevel.Error, telemetryContext*)
    def fatal(label: String, telemetryContext: (String, Any)*)(implicit trace: Trace): ZIO[R, E, A] = apply(label, Logger.LogLevel.Fatal, telemetryContext*)
    def always(label: String, telemetryContext: (String, Any)*)(implicit trace: Trace): ZIO[R, E, A] = apply(label, Logger.LogLevel.Always, telemetryContext*)

  }

  sealed abstract class ErrorLogOps(
      doLog: (Logger.LogLevel, Cause[E], Seq[(String, Any)], Trace, ErrorLogger[E]) => UIO[Unit],
  ) {

    /**
      * If the [[Cause]] has a [[Cause.Fail]], log it, and succeed with None.
      */
    def failure(
        context: (String, Any)*,
    )(implicit trace: Trace, errorLogger: ErrorLogger[E]): URIO[R, Option[A]] =
      self.foldCauseZIO(
        _.failureTraceOrCause match {
          case Left((e, stackTrace)) => doLog(Logger.LogLevel.Error, Cause.Fail(e, stackTrace), context, trace, errorLogger).as(None)
          case Right(cause)          => ZIO.refailCause(cause)
        },
        ZIO.some,
      )

    /**
      * No matter the [[Cause]], log it, and succeed with None.
      */
    def cause(
        causeLevel: Logger.LogLevel,
        context: (String, Any)*,
    )(implicit trace: Trace, errorLogger: ErrorLogger[E]): URIO[R, Option[A]] =
      self.foldCauseZIO(
        doLog(causeLevel, _, context, trace, errorLogger).as(None),
        ZIO.some,
      )

    /**
      * No matter the [[Cause]], log it, and succeed with None.
      * If the [[Cause]] contains a [[Cause.Fail]], only that cause will be logged, instead of all potential causes.
      */
    def simpleCause(
        causeLevel: Logger.LogLevel,
        context: (String, Any)*,
    )(implicit trace: Trace, errorLogger: ErrorLogger[E]): URIO[R, Option[A]] =
      self.foldCauseZIO(
        cause => doLog(causeLevel, cause.failureTraceOption.fold(cause) { case (e, stackTrace) => Cause.Fail(e, stackTrace) }, context, trace, errorLogger).as(None),
        ZIO.some,
      )

  }

  object logErrorDiscard extends ErrorLogOps((causeLevel, cause, context, trace, errorLogger) => Logger.logCause(causeLevel)(cause, context*)(using trace, errorLogger))
  object logErrorStackDiscard extends ErrorLogOps((causeLevel, cause, context, trace, errorLogger) => Logger.logCauseStack(causeLevel)(cause, context*)(using trace, errorLogger))

}

// =====| Schedule |=====

implicit class ScheduleOps[State0, Env, In, Out](self: Schedule.WithState[State0, Env, In, Out]) {

  // ---  ---

  def withMax(max: Out)(implicit ord: Ordering[Out]): Schedule.WithState[State0, Env, In, Out] =
    self.map(ord.min(_, max))

  def resetUnless(f: Out => Boolean): Schedule.WithState[State0, Env, In, Out] =
    self.resetWhen(!f(_))

  def resetWhenI[In2 <: In](f: In2 => Boolean): Schedule.WithState[(State0, Unit), Env, In2, Out] =
    (self <*> Schedule.identity[In2])
      .resetWhen { case (_, i) => f(i) }
      .map(_._1)

  def resetUnlessI[In2 <: In](f: In2 => Boolean): Schedule.WithState[(State0, Unit), Env, In2, Out] =
    resetWhenI(!f(_))

  // ---  ---

  def withMaxDelay(max: Duration): Schedule.WithState[State0, Env, In, Out] =
    self.modifyDelay { (_, delay) => delay.min(max) }

  def removeDelays: Schedule.WithState[State0, Env, In, Out] =
    new Schedule[Env, In, Out] {
      override type State = State0
      override def initial: State0 = self.initial
      override def step(now: OffsetDateTime, in: In, state: State0)(implicit trace: Trace): ZIO[Env, Nothing, (State0, Out, Schedule.Decision)] =
        self.step(now, in, state).map {
          case res @ (_, _, Schedule.Decision.Done)        => res
          case (state, out, Schedule.Decision.Continue(_)) => (state, out, Schedule.Decision.Continue(Schedule.Interval.after(now)))
        }
    }

}

implicit class ScheduleDurationOps[State0, Env, In](self: Schedule.WithState[State0, Env, In, Duration]) {

  def delayed0: Schedule.WithState[State0, Env, In, Duration] = Schedule.delayed(self)

}

implicit class ScheduleCompanionOps(self: Schedule.type) {

  def fibonacciNoWait(one: Duration): Schedule.WithState[(Duration, Duration), Any, Any, Duration] =
    Schedule
      .unfold[(Duration, Duration)]((one, one)) { case (a1, a2) => (a2, a1 + a2) }
      .map(_._1)

  def fibonacciMax(one: Duration, max: Duration): Schedule.WithState[(Duration, Duration), Any, Any, Duration] =
    Schedule.fibonacciNoWait(one).withMax(max).delayed0

  /**
    * Similar to [[Schedule.fromDurations]], except instead of finishing when `durations` runs out, it will stay forever on the last duration.
    */
  def fromDurationsForever(duration: Duration, durations: Duration*): Schedule.WithState[::[Duration], Any, Any, Duration] =
    new Schedule[Any, Any, Duration] {
      override type State = ::[Duration]
      override final val initial: State = ::(duration, durations.toList)
      override final def step(now: OffsetDateTime, in: Any, state: State)(implicit trace: Trace): ZIO[Any, Nothing, (State, Duration, Schedule.Decision)] =
        ZIO.succeed {
          val h :: r = state
          (
            r match {
              case h2 :: r2 => ::(h2, r2)
              case Nil      => state
            },
            h,
            Schedule.Decision.Continue(Schedule.Interval.after(now.plusNanos(h.toNanos))),
          )
        }
    }

}
