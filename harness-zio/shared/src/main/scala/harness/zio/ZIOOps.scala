package harness.zio

import cats.data.{EitherNel, NonEmptyList}
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
  def hFailUserErrors(string0: String, stringN: String*): HTask[Nothing] = hFailUserErrors(NonEmptyList(string0, stringN.toList))

  def hFailInternalDefect(string: String): HTask[Nothing] = ZIO.fail(HError.InternalDefect(string))
  def hFailInternalDefects(strings: NonEmptyList[String]): HTask[Nothing] = ZIO.fail(HError(strings.map(HError.InternalDefect(_))))
  def hFailInternalDefects(string0: String, stringN: String*): HTask[Nothing] = hFailInternalDefects(NonEmptyList(string0, stringN.toList))

  def eitherNelToUserErrors[T](eitherNel: EitherNel[String, T]): HTask[T] =
    eitherNel match {
      case Right(value) => ZIO.succeed(value)
      case Left(errors) => ZIO.hFailUserErrors(errors)
    }

  def eitherNelToInternalDefects[T](eitherNel: EitherNel[String, T]): HTask[T] =
    eitherNel match {
      case Right(value) => ZIO.succeed(value)
      case Left(errors) => ZIO.hFailInternalDefects(errors)
    }

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

extension [R, A](zio: HZIO[R, Nothing, A]) {
  def removeErrorOr: HRIO[R, A] =
    zio.mapError {
      case error: HError           => error
      case HError.Or(error, cause) => throw new RuntimeException(s"zio.removeErrorOr : should not be possible... ($error, $cause)")
    }
}
extension [R, A](zLayer: HZLayer[R, Nothing, A]) {
  def removeErrorOr: HRLayer[R, A] =
    zLayer.mapError {
      case error: HError           => error
      case HError.Or(error, cause) => throw new RuntimeException(s"zLayer.removeErrorOr : should not be possible... ($error, $cause)")
    }
}
extension [R, A](zStream: HZStream[R, Nothing, A]) {
  def removeErrorOr: HRStream[R, A] =
    zStream.mapError {
      case error: HError           => error
      case HError.Or(error, cause) => throw new RuntimeException(s"zStream.removeErrorOr : should not be possible... ($error, $cause)")
    }
}

extension [R, E, A](zio: HZIO[R, E, A]) {
  def widenE[E2 >: E]: HZIO[R, E2, A] = zio
  def mapErrorOr[E2](f: E => E2): HZIO[R, E2, A] =
    zio.mapError {
      case hError: HError           => hError
      case or @ HError.Or(error, _) => HError.Or(f(error), or)
    }
}
extension [R, E, A](zLayer: HZLayer[R, E, A]) {
  def widenE[E2 >: E]: HZLayer[R, E2, A] = zLayer
  def mapErrorOr[E2](f: E => E2): HZLayer[R, E2, A] =
    zLayer.mapError {
      case hError: HError           => hError
      case or @ HError.Or(error, _) => HError.Or(f(error), or)
    }
}
extension [R, E, A](zStream: HZStream[R, E, A]) {
  def widenE[E2 >: E]: HZStream[R, E2, A] = zStream
  def mapErrorOr[E2](f: E => E2): HZStream[R, E2, A] =
    zStream.mapError {
      case hError: HError           => hError
      case or @ HError.Or(error, _) => HError.Or(f(error), or)
    }
}

// =====| Error Recovery |=====

extension [R, E, A](zio: HZIO[R, E, A]) {
  def recoverFromErrorOr[A2 >: A](f: E => A2): HRIO[R, A2] =
    zio.foldZIO(
      {
        case hError: HError      => ZIO.fail(hError)
        case HError.Or(error, _) => ZIO.succeed(f(error))
      },
      ZIO.succeed(_),
    )
  def recoverFromErrorOrZIO[R2, E2 >: HError <: AnyHError, A2 >: A](f: E => ZIO[R2, E2, A2]): ZIO[R & R2, E2, A2] =
    zio.foldZIO(
      {
        case hError: HError      => ZIO.fail(hError)
        case HError.Or(error, _) => f(error)
      },
      ZIO.succeed(_),
    )
}

// =====| Logging |=====

extension [R, E, A](self: ZIO[R, E, A]) {
  inline def telemetrize(label: String, telemetryContext: (String, Any)*): ZIO[Telemetry & Logger & R, E, A] =
    self.telemetrize(label, Logger.LogLevel.Trace, telemetryContext*)
  def telemetrize(label: String, logLevel: Logger.LogLevel, telemetryContext: (String, Any)*): ZIO[Telemetry & Logger & R, E, A] =
    Telemetry.telemetrize(self, label, logLevel, telemetryContext.map { (k, v) => (k, String.valueOf(v)) }.toMap)
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
