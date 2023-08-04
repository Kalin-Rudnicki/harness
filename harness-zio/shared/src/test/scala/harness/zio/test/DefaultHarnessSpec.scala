package harness.zio.test

import zio.*

abstract class DefaultHarnessSpec extends HarnessSpec[Any] {
  override final val rLayer: ULayer[Any] = ZLayer.empty
}
