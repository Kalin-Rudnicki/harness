package harness.zio

import cats.data.NonEmptyList
import harness.core.*
import zio.*

extension (self: ZIO.type) {

  def hAttempt[A](mapError: Throwable => KError)(thunk: => A): HTask[A] =
    ZIO.attempt(thunk).mapError(mapError)

  def hAttempt[A](internalMessage: => String)(thunk: => A): HTask[A] =
    ZIO.hAttempt(KError.InternalDefect(internalMessage, _))(thunk)

  def failNel[E](fail0: E, failN: E*): IO[NonEmptyList[E], Nothing] =
    ZIO.fail(NonEmptyList(fail0, failN.toList))

}

extension [R, A](zio: HRIO[R, A]) {

  def orDieH: URIO[R, A] =
    zio.orDie

}
