package harness.zio

import zio.*

type HarnessEnv = Logger & Telemetry & FileSystem & HConfig
object HarnessEnv {

  def defaultLayer: ULayer[HarnessEnv] =
    ZLayer.succeed(Logger.default()) ++
      ZLayer.succeed(Telemetry.log) ++
      FileSystem.liveLayer.orDie ++
      HConfig.layer.empty

  def defaultLayer(logLevel: Logger.LogLevel): ULayer[HarnessEnv] =
    ZLayer.succeed(Logger.default(defaultMinLogTolerance = logLevel)) ++
      ZLayer.succeed(Telemetry.log) ++
      FileSystem.liveLayer.orDie ++
      HConfig.layer.empty

}
