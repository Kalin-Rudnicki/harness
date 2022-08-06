package harness.zio

import zio.*

type HarnessEnv = Logger & RunMode
object HarnessEnv {

  val defaultLayer: ULayer[HarnessEnv] =
    ZLayer.succeed(Logger(Logger.Source.stdOut(Logger.LogLevel.Info, Logger.LogLevel.Always) :: Nil)) ++
      ZLayer.succeed(RunMode.Prod)

}
