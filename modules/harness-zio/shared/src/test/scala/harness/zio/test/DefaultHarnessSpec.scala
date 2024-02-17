package harness.zio.test

import zio.*

abstract class DefaultHarnessSpec extends HarnessSpec[Any] {
  override final val layer: ULayer[Any] = ZLayer.empty
}
