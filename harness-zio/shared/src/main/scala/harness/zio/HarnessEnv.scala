package harness.zio

import harness.core.*
import zio.*

type HarnessEnv = Logger & RunMode & HError.UserMessage.IfHidden & FileSystem
object HarnessEnv {

  val defaultLayer: HTaskLayer[HarnessEnv] =
    ZLayer.succeed(Logger.default()) ++
      ZLayer.succeed(RunMode.Prod) ++
      ZLayer.succeed(HError.UserMessage.IfHidden.default) ++
      FileSystem.liveLayer

}
