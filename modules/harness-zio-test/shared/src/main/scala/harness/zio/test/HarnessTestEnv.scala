package harness.zio.test

import harness.zio.*
import zio.*

type HarnessTestEnv = LogCache
object HarnessTestEnv {

  val layer: ULayer[HarnessTestEnv] =
    ZLayer.make[HarnessTestEnv](
      LogCache.layer,
    )

}
