package harness.zio.test

import zio.*

abstract class ZioDefaultHarnessSpec extends ZioHarnessSpec[Any] {
  override final val layer: ULayer[Any] = ZLayer.empty
}
