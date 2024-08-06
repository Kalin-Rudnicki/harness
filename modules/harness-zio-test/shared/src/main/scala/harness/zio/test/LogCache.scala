package harness.zio.test

import harness.zio.*
import java.time.Instant
import zio.*

trait LogCache {

  // =====| Trace |=====
  // This will capture all log events

  def addTrace(event: Logger.ExecutedEvent): UIO[Unit]
  def addTrace(events: Chunk[Logger.ExecutedEvent]): UIO[Unit]
  def getTrace: UIO[Chunk[Logger.ExecutedEvent]]

  final def existsTrace(
      message: String => Boolean = _ => true,
      logLevel: Option[Logger.LogLevel] => Boolean = _ => true,
      context: Map[String, String] => Boolean = _ => true,
      at: Instant => Boolean = _ => true,
  ): UIO[Boolean] =
    getTrace.map { _.exists { l => message(l.message) && logLevel(l.logLevel) && context(l.context.toMap) && at(l.timestamp) } }

  // =====| Default |=====
  // This will capture only log events >= the loggers current default

  def addDefault(event: Logger.ExecutedEvent): UIO[Unit]
  def addDefault(events: Chunk[Logger.ExecutedEvent]): UIO[Unit]
  def getDefault: UIO[Chunk[Logger.ExecutedEvent]]

  final def existsDefault(
      message: String => Boolean = _ => true,
      logLevel: Option[Logger.LogLevel] => Boolean = _ => true,
      context: Map[String, String] => Boolean = _ => true,
      at: Instant => Boolean = _ => true,
  ): UIO[Boolean] =
    getDefault.map { _.exists { l => message(l.message) && logLevel(l.logLevel) && context(l.context.toMap) && at(l.timestamp) } }

}
object LogCache {

  val layer: ULayer[LogCache] =
    ZLayer.fromZIO {
      for {
        traceRef <- Ref.make(Chunk.empty[Logger.ExecutedEvent])
        defaultRef <- Ref.make(Chunk.empty[Logger.ExecutedEvent])
      } yield LogCache.Impl(traceRef = traceRef, defaultRef = defaultRef)
    }

  // =====| API |=====

  def addTrace(event: Logger.ExecutedEvent): URIO[LogCache, Unit] = ZIO.serviceWithZIO[LogCache](_.addTrace(event))
  def addTrace(events: Chunk[Logger.ExecutedEvent]): URIO[LogCache, Unit] = ZIO.serviceWithZIO[LogCache](_.addTrace(events))
  def getTrace: URIO[LogCache, Chunk[Logger.ExecutedEvent]] = ZIO.serviceWithZIO[LogCache](_.getTrace)

  final def existsTrace(
      message: String => Boolean = _ => true,
      logLevel: Option[Logger.LogLevel] => Boolean = _ => true,
      context: Map[String, String] => Boolean = _ => true,
      at: Instant => Boolean = _ => true,
  ): URIO[LogCache, Boolean] =
    ZIO.serviceWithZIO[LogCache](_.existsTrace(message, logLevel, context, at))

  def addDefault(event: Logger.ExecutedEvent): URIO[LogCache, Unit] = ZIO.serviceWithZIO[LogCache](_.addDefault(event))
  def addDefault(events: Chunk[Logger.ExecutedEvent]): URIO[LogCache, Unit] = ZIO.serviceWithZIO[LogCache](_.addDefault(events))
  def getDefault: URIO[LogCache, Chunk[Logger.ExecutedEvent]] = ZIO.serviceWithZIO[LogCache](_.getDefault)

  final def existsDefault(
      message: String => Boolean = _ => true,
      logLevel: Option[Logger.LogLevel] => Boolean = _ => true,
      context: Map[String, String] => Boolean = _ => true,
      at: Instant => Boolean = _ => true,
  ): URIO[LogCache, Boolean] =
    ZIO.serviceWithZIO[LogCache](_.existsDefault(message, logLevel, context, at))

  // =====|  |=====

  final case class Impl(
      traceRef: Ref[Chunk[Logger.ExecutedEvent]],
      defaultRef: Ref[Chunk[Logger.ExecutedEvent]],
  ) extends LogCache {

    override def addTrace(event: Logger.ExecutedEvent): UIO[Unit] = traceRef.update { _ :+ event }
    override def addTrace(events: Chunk[Logger.ExecutedEvent]): UIO[Unit] = traceRef.update { _ ++ events }
    override def getTrace: UIO[Chunk[Logger.ExecutedEvent]] = traceRef.get

    override def addDefault(event: Logger.ExecutedEvent): UIO[Unit] = defaultRef.update { _ :+ event }
    override def addDefault(events: Chunk[Logger.ExecutedEvent]): UIO[Unit] = defaultRef.update { _ ++ events }
    override def getDefault: UIO[Chunk[Logger.ExecutedEvent]] = defaultRef.get

  }

  final case class LoggerTarget(logCache: LogCache, use: (LogCache, Logger.ExecutedEvent) => UIO[Unit]) extends Logger.Target {
    override def log(event: Logger.ExecutedEvent): UIO[Unit] = use(logCache, event)
  }

}
