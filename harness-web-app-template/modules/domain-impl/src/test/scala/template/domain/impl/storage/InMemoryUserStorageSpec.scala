package template.domain.impl.storage

import harness.zio.test.*
import template.domain.impl.storage.inMemory.*
import template.domain.storage.*
import zio.*

object InMemoryUserStorageSpec extends ContractHarnessSpec[UserStorage]("InMemoryUserStorage", UserStorageContract) {

  override def layerProvider: LayerProvider[R] =
    LayerProvider
      .providePerTest(
        DbState.layer,
        InMemoryUserStorage.layer,
      )

}
