package harness.zio

import cats.data.NonEmptyList
import cats.syntax.list.*
import harness.core.*
import scala.annotation.tailrec
import zio.*

// =====| ZIO._ |=====

extension (self: ZIO.type) {

  def traverse[R, A, B](nel: NonEmptyList[A])(f: A => HRIO[R, B]): HRIO[R, NonEmptyList[B]] =
    ZIO
      .foreach(NonEmptyChunk(nel.head, nel.tail*))(f(_).either)
      .flatMap { nec =>
        @tailrec
        def loopErrors(
            queue: List[Either[HError, B]],
            errors: NonEmptyList[HError],
        ): HTask[Nothing] =
          queue match {
            case head :: tail =>
              head match {
                case Right(_)    => loopErrors(tail, errors)
                case Left(error) => loopErrors(tail, error :: errors)
              }
            case Nil =>
              ZIO.fail(HError(errors.reverse))
          }

        @tailrec
        def loopValues(
            queue: List[Either[HError, B]],
            values: NonEmptyList[B],
        ): HTask[NonEmptyList[B]] =
          queue match {
            case head :: tail =>
              head match {
                case Right(value) => loopValues(tail, value :: values)
                case Left(error)  => loopErrors(tail, NonEmptyList.one(error))
              }
            case Nil =>
              ZIO.succeed(values.reverse)
          }

        nec.head match {
          case Right(value) => loopValues(nec.tail.toList, NonEmptyList.one(value))
          case Left(error)  => loopErrors(nec.tail.toList, NonEmptyList.one(error))
        }
      }

  def traverse[R, A, B](list: List[A])(f: A => HRIO[R, B]): HRIO[R, List[B]] =
    list.toNel match {
      case Some(nel) => traverse(nel)(f).map(_.toList)
      case None      => ZIO.succeed(Nil)
    }

  def hAttempt[A](thunk: => A): HTask[A] =
    ZIO.attempt(thunk).wrapHError

  def acquireClosable[R, E, A <: java.io.Closeable](acq: => ZIO[R, E, A]): ZIO[R & Scope, E, A] =
    ZIO.acquireRelease(acq)(c => ZIO.attempt(c.close()).orDie)
  def acquireAutoClosable[R, E, A <: java.lang.AutoCloseable](acq: => ZIO[R, E, A]): ZIO[R & Scope, E, A] =
    ZIO.acquireRelease(acq)(c => ZIO.attempt(c.close()).orDie)

  def hFailUserError(string: String): HTask[Nothing] = ZIO.fail(HError.UserError(string))
  def hFailUserErrors(strings: NonEmptyList[String]): HTask[Nothing] = ZIO.fail(HError(strings.map(HError.UserError(_))))

}

// =====| Error Mapping |=====

private def causeToHError(cause: Cause[HError]): Cause.Fail[HError] =
  Cause.Fail[HError](
    cause match {
      case Cause.Fail(hError, _)       => hError
      case Cause.Die(throwable, _)     => HError.InternalDefect("ZIO died", throwable)
      case Cause.Interrupt(fiberId, _) => HError.InternalDefect(s"ZIO was interrupted (fiberId: $fiberId)")
      case Cause.Then(left, right)     => HError(causeToHError(left).value, causeToHError(right).value)
      case Cause.Both(left, right)     => HError(causeToHError(left).value, causeToHError(right).value)
      case Cause.Stackless(cause, _)   => causeToHError(cause).value
      case Cause.Empty                 => HError.InternalDefect("ZIO failed with no cause")
    },
    cause.trace,
  )

extension [R, A](zio: RIO[R, A]) {
  def wrapHError: HRIO[R, A] = zio.mapError(HError.fromThrowable)
}
extension [R, A](zLayer: RLayer[R, A]) {
  def wrapHError: HRLayer[R, A] = zLayer.mapError(HError.fromThrowable)
}
extension [R, A](zStream: RStream[R, A]) {
  def wrapHError: HRStream[R, A] = zStream.mapError(HError.fromThrowable)
}

// =====| Logging |=====

extension [R, E, A](self: ZIO[R, E, A]) {
  inline def trace(label: String, telemetryContext: (String, Any)*): ZIO[Telemetry & Logger & R, E, A] = self.trace(label, Logger.LogLevel.Trace, telemetryContext*)
  def trace(label: String, logLevel: Logger.LogLevel, telemetryContext: (String, Any)*): ZIO[Telemetry & Logger & R, E, A] =
    Telemetry.trace(self, label, logLevel, telemetryContext.map { (k, v) => (k, String.valueOf(v)) }.toMap)
}

extension [R, A](self: HRIO[R, A]) {

  def collapseCause: HRIO[R, A] = self.mapErrorCause(causeToHError)

  def dumpErrorsAndContinue: URIO[R & RunMode & Logger & HError.UserMessage.IfHidden, Option[A]] =
    self.dumpErrorsAndContinue(Logger.LogLevel.Error)

  def dumpErrorsAndContinue(errorLevel: Logger.LogLevel): URIO[R & RunMode & Logger & HError.UserMessage.IfHidden, Option[A]] =
    self.collapseCause.foldZIO(
      error => Logger.logHError(errorLevel, error).as(None),
      ZIO.some,
    )

  def hZipWith[R2, B, C](other: HRIO[R2, B])(f: (A, B) => C): HRIO[R & R2, C] =
    (self.either <*> other.either).flatMap {
      case (Right(a), Right(b)) => ZIO.succeed(f(a, b))
      case (Left(a), Right(_))  => ZIO.fail(a)
      case (Right(_), Left(b))  => ZIO.fail(b)
      case (Left(a), Left(b))   => ZIO.fail(HError(a, b))
    }

  def <**>[R2, B](other: HRIO[R2, B])(implicit zip: Zippable[A, B]): HRIO[R & R2, zip.Out] = self.hZipWith(other)(zip.zip)
  def <**[R2, B](other: HRIO[R2, B]): HRIO[R & R2, A] = self.hZipWith(other)((a, _) => a)
  def **>[R2, B](other: HRIO[R2, B]): HRIO[R & R2, B] = self.hZipWith(other)((_, b) => b)

  def hZipWithPar[R2, B, C](other: HRIO[R2, B])(f: (A, B) => C): HRIO[R & R2, C] =
    (self.either <&> other.either).flatMap {
      case (Right(a), Right(b)) => ZIO.succeed(f(a, b))
      case (Left(a), Right(_))  => ZIO.fail(a)
      case (Right(_), Left(b))  => ZIO.fail(b)
      case (Left(a), Left(b))   => ZIO.fail(HError(a, b))
    }

  def <&&>[R2, B](other: HRIO[R2, B])(implicit zip: Zippable[A, B]): HRIO[R & R2, zip.Out] = self.hZipWithPar(other)(zip.zip)
  def <&&[R2, B](other: HRIO[R2, B]): HRIO[R & R2, A] = self.hZipWithPar(other)((a, _) => a)
  def &&>[R2, B](other: HRIO[R2, B]): HRIO[R & R2, B] = self.hZipWithPar(other)((_, b) => b)

}

extension [R, A](self: HRLayer[R, A]) {
  def collapseCause: HRLayer[R, A] = self.mapErrorCause(causeToHError)
}

extension [R, A](self: HRStream[R, A]) {
  def collapseCause: HRStream[R, A] = self.mapErrorCause(causeToHError)
}
