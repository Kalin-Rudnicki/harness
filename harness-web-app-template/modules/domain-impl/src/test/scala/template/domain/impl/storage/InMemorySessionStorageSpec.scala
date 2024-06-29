package template.domain.impl.storage

import harness.zio.test.*
import template.domain.impl.storage.inMemory.*
import template.domain.storage.*
import zio.*

object InMemorySessionStorageSpec extends ContractHarnessSpec[UserStorage & SessionStorage]("InMemorySessionStorage", SessionStorageContract) {

  override def layerProvider: LayerProvider[R] =
    LayerProvider
      .providePerTest(
        DbState.layer,
        InMemoryUserStorage.layer,
        InMemorySessionStorage.layer,
      )

}
