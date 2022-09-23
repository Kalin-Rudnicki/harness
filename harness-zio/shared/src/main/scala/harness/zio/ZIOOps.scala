package harness.zio

import cats.data.NonEmptyList
import cats.syntax.list.*
import harness.core.*
import scala.annotation.tailrec
import zio.*

// =====| ZIO._ |=====

extension (self: ZIO.type) {

  def traverse[R, E, A, B](nel: NonEmptyList[A])(f: A => ZIO[R, E, B]): ZIO[R, NonEmptyList[E], NonEmptyList[B]] =
    ZIO
      .foreach(NonEmptyChunk(nel.head, nel.tail*))(f(_).either)
      .flatMap { nec =>
        @tailrec
        def loopErrors(
            queue: List[Either[E, B]],
            errors: NonEmptyList[E],
        ): IO[NonEmptyList[E], Nothing] =
          queue match {
            case head :: tail =>
              head match {
                case Right(_)    => loopErrors(tail, errors)
                case Left(error) => loopErrors(tail, error :: errors)
              }
            case Nil =>
              ZIO.fail(errors.reverse)
          }

        @tailrec
        def loopValues(
            queue: List[Either[E, B]],
            values: NonEmptyList[B],
        ): IO[NonEmptyList[E], NonEmptyList[B]] =
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
  def traverseNel[R, E, A, B](nel: NonEmptyList[A])(f: A => ZIO[R, NonEmptyList[E], B]): ZIO[R, NonEmptyList[E], NonEmptyList[B]] =
    traverse(nel)(f).mapError(_.flatMap(identity))

  def traverse[R, E, A, B](list: List[A])(f: A => ZIO[R, E, B]): ZIO[R, NonEmptyList[E], List[B]] =
    list.toNel match {
      case Some(nel) => traverse(nel)(f).map(_.toList)
      case None      => ZIO.succeed(Nil)
    }
  def traverseNel[R, E, A, B](list: List[A])(f: A => ZIO[R, NonEmptyList[E], B]): ZIO[R, NonEmptyList[E], List[B]] =
    traverse(list)(f).mapError(_.flatMap(identity))

  def hAttempt[A](mapError: Throwable => HError)(thunk: => A): HTask[A] =
    ZIO.attempt(thunk).mapError(mapError)

  def hAttempt[A](errorType: => HError.ErrorType, msg: => String)(thunk: => A): HTask[A] =
    ZIO.hAttempt(HError(errorType)(msg, _))(thunk)

  def hAttempt[A](internalMessage: => String)(thunk: => A): HTask[A] =
    ZIO.hAttempt(HError.InternalDefect(internalMessage, _))(thunk)

  def hAttemptNel[A](mapError: Throwable => HError)(thunk: => A): HTaskN[A] =
    ZIO.attempt(thunk).mapError(mapError).toErrorNel

  def hAttemptNel[A](errorType: => HError.ErrorType, msg: => String)(thunk: => A): HTaskN[A] =
    ZIO.hAttempt(HError(errorType)(msg, _))(thunk).toErrorNel

  def hAttemptNel[A](internalMessage: => String)(thunk: => A): HTaskN[A] =
    ZIO.hAttempt(HError.InternalDefect(internalMessage, _))(thunk).toErrorNel

  def failNel[E](fail0: E, failN: E*): IO[NonEmptyList[E], Nothing] =
    ZIO.fail(NonEmptyList(fail0, failN.toList))

  def acquireClosable[R, E, A <: java.io.Closeable](acq: => ZIO[R, E, A]): ZIO[R & Scope, E, A] =
    ZIO.acquireRelease(acq)(c => ZIO.attempt(c.close()).orDie)

}

// =====| Error Mapping |=====

extension [R, A](zio: HRIO[R, A]) {

  def orDieH: URIO[R, A] =
    zio.orDie

}

extension [R, E, A](zio: ZIO[R, E, A]) {

  def mapErrorToNel[E2](f: E => E2): ZIO[R, NonEmptyList[E2], A] =
    zio.mapError(e => NonEmptyList.one(f(e)))

  def toErrorNel: ZIO[R, NonEmptyList[E], A] =
    zio.mapError(NonEmptyList.one)

}
extension [R, E, A](zLayer: ZLayer[R, E, A]) {

  def mapErrorToNel[E2](f: E => E2): ZLayer[R, NonEmptyList[E2], A] =
    zLayer.mapError(e => NonEmptyList.one(f(e)))

  def toErrorNel: ZLayer[R, NonEmptyList[E], A] =
    zLayer.mapError(NonEmptyList.one)

}

extension [R, E, A](zio: ZIO[R, NonEmptyList[E], A]) {

  def mapErrorNel[E2](f: E => E2): ZIO[R, NonEmptyList[E2], A] =
    zio.mapError(_.map(f))

}
extension [R, E, A](zLayer: ZLayer[R, NonEmptyList[E], A]) {

  def mapErrorNel[E2](f: E => E2): ZLayer[R, NonEmptyList[E2], A] =
    zLayer.mapError(_.map(f))

}

// =====| Logging |=====

extension [R, A](self: HRIO[R, A]) {

  def dumpErrorsAndContinue: URIO[R & RunMode & Logger, Option[A]] =
    self.dumpErrorsAndContinue(Logger.LogLevel.Error)

  def dumpErrorsAndContinue(errorLevel: Logger.LogLevel): URIO[R & RunMode & Logger, Option[A]] =
    self.foldZIO(
      error => Logger.logHError(errorLevel, error).as(None),
      ZIO.some,
    )

}

extension [R, A](self: HRION[R, A]) {

  def dumpErrorsAndContinueNel: URIO[R & RunMode & Logger, Option[A]] =
    self.dumpErrorsAndContinueNel(Logger.LogLevel.Error)

  def dumpErrorsAndContinueNel(errorLevel: Logger.LogLevel): URIO[R & RunMode & Logger, Option[A]] =
    self.foldZIO(
      errors => ZIO.foreachDiscard(errors.toList)(error => Logger.logHError(errorLevel, error)).as(None),
      ZIO.some,
    )

}
