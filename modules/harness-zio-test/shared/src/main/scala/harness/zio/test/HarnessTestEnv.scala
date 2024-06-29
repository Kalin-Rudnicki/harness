package harness.zio.test

import cats.syntax.option.*
import harness.core.*
import harness.zio.*
import zio.*

type HarnessTestEnv = LogCache
object HarnessTestEnv {

  private val loggerLayer: URLayer[LogCache.LoggerTarget, Logger] =
    ZLayer.fromFunction { (logCacheTarget: LogCache.LoggerTarget) =>
      Logger.default(
        sources = List(
          Logger.Source.const(logCacheTarget, Logger.LogLevel.Trace.some),
          Logger.Source.stdOut(None, ColorMode.Extended),
        ),
        defaultMinLogTolerance = Logger.LogLevel.Important,
      )
    }

  val layer: ULayer[HarnessEnv & HarnessTestEnv] =
    ZLayer.make[HarnessEnv & HarnessTestEnv](
      LogCache.layer,
      loggerLayer,
      ZLayer.succeed(Telemetry.log),
      FileSystem.liveLayer.orDie, // TODO (KR) : mock
      Sys.liveLayer(true),
      HConfig.layer.empty, // TODO (KR) : configurable?
    )

}
