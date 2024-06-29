package harness.zio.test

import harness.zio.*
import java.time.Instant
import zio.*

trait LogCache {

  def add(event: Logger.ExecutedEvent): UIO[Unit]
  def add(events: Chunk[Logger.ExecutedEvent]): UIO[Unit]

  def get: UIO[Chunk[Logger.ExecutedEvent]]

  final def exists(
      message: String => Boolean = _ => true,
      logLevel: Option[Logger.LogLevel] => Boolean = _ => true,
      context: Map[String, String] => Boolean = _ => true,
      at: Instant => Boolean = _ => true,
  ): UIO[Boolean] =
    get.map { _.exists { l => message(l.message) && logLevel(l.logLevel) && context(l.context) && at(l.at) } }

}
object LogCache {

  val layer: ULayer[LogCache & LoggerTarget] = LogCache.Impl.layer >+> LogCache.LoggerTarget.layer

  // =====| API |=====

  def add(event: Logger.ExecutedEvent): URIO[LogCache, Unit] = ZIO.serviceWithZIO[LogCache](_.add(event))
  def add(events: Chunk[Logger.ExecutedEvent]): URIO[LogCache, Unit] = ZIO.serviceWithZIO[LogCache](_.add(events))
  def get: URIO[LogCache, Chunk[Logger.ExecutedEvent]] = ZIO.serviceWithZIO[LogCache](_.get)

  final def exists(
      message: String => Boolean = _ => true,
      logLevel: Option[Logger.LogLevel] => Boolean = _ => true,
      context: Map[String, String] => Boolean = _ => true,
      at: Instant => Boolean = _ => true,
  ): URIO[LogCache, Boolean] =
    ZIO.serviceWithZIO[LogCache](_.exists(message, logLevel, context, at))

  // =====|  |=====

  final case class Impl(ref: Ref[Chunk[Logger.ExecutedEvent]]) extends LogCache {
    override def add(event: Logger.ExecutedEvent): UIO[Unit] = ref.update { _ :+ event }
    override def add(events: Chunk[Logger.ExecutedEvent]): UIO[Unit] = ref.update { _ ++ events }
    override def get: UIO[Chunk[Logger.ExecutedEvent]] = ref.get
  }
  object Impl {
    val layer: ULayer[LogCache] = ZLayer.fromZIO { Ref.make(Chunk.empty[Logger.ExecutedEvent]).map(Impl(_)) }
  }

  final case class LoggerTarget(logCache: LogCache) extends Logger.Target {
    override def log(event: Logger.ExecutedEvent): UIO[Unit] = logCache.add(event)
  }
  object LoggerTarget {
    val layer: URLayer[LogCache, LoggerTarget] = ZLayer.fromFunction { LoggerTarget.apply }
  }

}
