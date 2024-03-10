package harness.archive.domain.impl.storage

import harness.zio.test.*
import harness.archive.domain.impl.storage.inMemory.*
import harness.archive.domain.storage.*
import zio.*

object InMemorySessionStorageSpec
    extends DefaultHarnessSpec.ForContract[UserStorage & SessionStorage]("InMemorySessionStorage", SessionStorageContract)(
      ZLayer.make[UserStorage & SessionStorage](
        DbState.layer,
        InMemoryUserStorage.layer,
        InMemorySessionStorage.layer,
      ),
    )
