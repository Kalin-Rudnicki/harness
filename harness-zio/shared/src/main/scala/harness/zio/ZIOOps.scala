package harness.zio

import harness.core.*
import zio.*

extension (self: ZIO.type) {

  def hAttempt[A](mapError: Throwable => KError)(thunk: => A): HTask[A] =
    ZIO.attempt(thunk).mapError(mapError)

  def hAttempt[A](internalMessage: => String)(thunk: => A): HTask[A] =
    ZIO.hAttempt(KError.InternalDefect(internalMessage, _))(thunk)

}

extension [R, A](zio: HRIO[R, A]) {

  def orDieH: URIO[R, A] =
    zio.orDie

}
