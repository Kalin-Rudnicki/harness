package harness.zio

import zio.*

final class LoggedZPool[Error, Item](wrapped: ZPool[Error, Item], label: String, context: Item => String, logLevel: Logger.LogLevel) extends ZPool[Error, Item] {

  override def get(implicit trace: Trace): ZIO[Scope, Error, Item] =
    wrapped.get
      .tap { item => Logger.log(logLevel)(label, "item" -> context(item), "action" -> "acquire") }
      .withFinalizer { item => Logger.log(logLevel)(label, "item" -> context(item), "action" -> "release") }

  override def invalidate(item: Item)(implicit trace: Trace): UIO[Unit] = wrapped.invalidate(item)

}
object LoggedZPool {

  def make[R, E, A](label: String, context: A => String, logLevel: Logger.LogLevel, get: => ZIO[R & Scope, E, A], size: => Int): ZIO[R & Scope, Nothing, ZPool[E, A]] =
    ZPool.make(
      get
        .tap { item => Logger.log(logLevel)(label, "item" -> context(item), "action" -> "open") }
        .withFinalizer { item => Logger.log(logLevel)(label, "item" -> context(item), "action" -> "close") },
      size,
    )

  def make[R, E, A](label: String, context: A => String, logLevel: Logger.LogLevel, get: => ZIO[R & Scope, E, A], range: => Range, timeToLive: => Duration): ZIO[R & Scope, Nothing, ZPool[E, A]] =
    ZPool.make(
      get
        .tap { item => Logger.log(logLevel)(label, "item" -> context(item), "action" -> "open") }
        .withFinalizer { item => Logger.log(logLevel)(label, "item" -> context(item), "action" -> "close") },
      range,
      timeToLive,
    )

}
