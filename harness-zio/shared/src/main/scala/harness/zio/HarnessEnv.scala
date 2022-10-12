package harness.zio

import harness.core.*
import zio.*

type HarnessEnv = Logger & RunMode & FileSystem
object HarnessEnv {

  val defaultLayer: Layer[HError, HarnessEnv] =
    ZLayer.succeed(Logger.default()) ++
      ZLayer.succeed(RunMode.Prod) ++
      FileSystem.liveLayer

}
