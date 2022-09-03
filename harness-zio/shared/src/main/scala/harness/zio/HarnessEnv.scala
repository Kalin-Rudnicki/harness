package harness.zio

import harness.core.*
import zio.*

type HarnessEnv = Logger & RunMode & FileSystem
object HarnessEnv {

  val defaultLayer: Layer[HError, HarnessEnv] =
    ZLayer.succeed(Logger(Logger.Source.stdOut(Logger.LogLevel.Info, Logger.LogLevel.Always) :: Nil)) ++
      ZLayer.succeed(RunMode.Prod) ++
      FileSystem.liveLayer

}
