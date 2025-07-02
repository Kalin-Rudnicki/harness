package harness.zio.test

import harness.zio.*

abstract class DefaultHarnessSpec extends HarnessSpec[Any] {
  override def layerProvider: LayerProvider[R] = LayerProvider.Empty
}
