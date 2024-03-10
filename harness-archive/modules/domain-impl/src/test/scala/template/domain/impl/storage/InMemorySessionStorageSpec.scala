package template.domain.impl.storage

import harness.zio.test.*
import template.domain.impl.storage.inMemory.*
import template.domain.storage.*
import zio.*

object InMemorySessionStorageSpec
    extends DefaultHarnessSpec.ForContract[UserStorage & SessionStorage]("InMemorySessionStorage", SessionStorageContract)(
      ZLayer.make[UserStorage & SessionStorage](
        DbState.layer,
        InMemoryUserStorage.layer,
        InMemorySessionStorage.layer,
      ),
    )
