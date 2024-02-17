package harness.zio

import cats.data.NonEmptyList
import cats.syntax.list.*
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

}

// =====| Cause |=====

implicit class CauseOps[E](self: Cause[E]) {

  def causeFailuresOpt: Option[NonEmptyList[Cause.Fail[E]]] =
    self
      .foldLeft(List.empty[Cause.Fail[E]]) { case (z, fail: Cause.Fail[E]) => fail :: z }
      .reverse
      .toNel

  def collapsedCauseFailures(implicit errorMapper: ErrorMapper[Throwable, E]): NonEmptyList[Cause.Fail[E]] =
    self.collapse.causeFailuresOpt match {
      case Some(value) => value
      case None        => throw new RuntimeException("collapsedCauseFailures = Nil, should not be possible")
    }

  def collapse(implicit errorMapper: ErrorMapper[Throwable, E]): Cause[E] =
    self match {
      case Cause.Empty                       => Cause.fail(errorMapper.mapError(new RuntimeException("Empty Cause")))
      case Cause.Fail(value, trace)          => Cause.Fail(value, trace)
      case Cause.Die(value, trace)           => Cause.Fail(errorMapper.mapError(value), trace)
      case Cause.Interrupt(fiberId, trace)   => Cause.Fail(errorMapper.mapError(new RuntimeException(s"Interrupted by fiber $fiberId")), trace)
      case Cause.Stackless(cause, stackless) => Cause.Stackless(cause.collapse, stackless)
      case Cause.Then(left, right)           => Cause.Then(left.collapse, right.collapse)
      case Cause.Both(left, right)           => Cause.Both(left.collapse, right.collapse)
    }

}

// =====| Logging / Telemetry |=====

implicit class ZIOLogTelemetryOps[R, E, A](self: ZIO[R, E, A]) {

  inline def telemetrize(label: String, telemetryContext: (String, Any)*): ZIO[Telemetry & Logger & R, E, A] =
    self.telemetrize(label, Logger.LogLevel.Trace, telemetryContext*)
  def telemetrize(label: String, logLevel: Logger.LogLevel, telemetryContext: (String, Any)*): ZIO[Telemetry & Logger & R, E, A] =
    Telemetry.telemetrize(self, label, logLevel, telemetryContext.map { (k, v) => (k, String.valueOf(v)) }.toMap)

  def logErrorAndContinue(context: (String, Any)*)(implicit errorLogger: ErrorLogger[E]): URIO[R & Logger, Option[A]] =
    self.foldZIO(
      Logger.logError(_, context*).as(None),
      ZIO.some,
    )

  def logErrorCauseAndContinue(
      causeLevel: Logger.LogLevel,
      stackTraceLevel: Option[Logger.LogLevel],
      context: (String, Any)*,
  )(implicit errorLogger: ErrorLogger[E]): URIO[R & Logger, Option[A]] =
    self.foldCauseZIO(
      Logger.logErrorCause(_, causeLevel, stackTraceLevel, context*).as(None),
      ZIO.some,
    )

  def logErrorCauseSimpleAndContinue(
      causeLevel: Logger.LogLevel,
      stackTraceLevel: Option[Logger.LogLevel],
      context: (String, Any)*,
  )(implicit errorLogger: ErrorLogger[E]): URIO[R & Logger, Option[A]] =
    self.foldCauseZIO(
      Logger.logErrorCauseSimple(_, causeLevel, stackTraceLevel, context*).as(None),
      ZIO.some,
    )

}
