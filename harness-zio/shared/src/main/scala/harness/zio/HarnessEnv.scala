package harness.zio

import harness.core.*
import zio.*

type HarnessEnv = Logger & Telemetry & RunMode & HError.UserMessage.IfHidden & FileSystem
object HarnessEnv {

  def defaultLayer: HTaskLayer[HarnessEnv] =
    ZLayer.succeed(Logger.default()) ++
      ZLayer.succeed(Telemetry.log) ++
      ZLayer.succeed(RunMode.Prod) ++
      ZLayer.succeed(HError.UserMessage.IfHidden.default) ++
      FileSystem.liveLayer

  def defaultLayer(logLevel: Logger.LogLevel): HTaskLayer[HarnessEnv] =
    ZLayer.succeed(Logger.default(defaultMinLogTolerance = logLevel)) ++
      ZLayer.succeed(Telemetry.log) ++
      ZLayer.succeed(RunMode.Prod) ++
      ZLayer.succeed(HError.UserMessage.IfHidden.default) ++
      FileSystem.liveLayer

}
