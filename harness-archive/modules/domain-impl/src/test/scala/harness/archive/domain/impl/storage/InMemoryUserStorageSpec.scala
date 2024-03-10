package harness.archive.domain.impl.storage

import harness.zio.test.*
import harness.archive.domain.impl.storage.inMemory.*
import harness.archive.domain.storage.*
import zio.*

object InMemoryUserStorageSpec
    extends DefaultHarnessSpec.ForContract[UserStorage]("InMemoryUserStorage", UserStorageContract)(
      ZLayer.make[UserStorage](
        DbState.layer,
        InMemoryUserStorage.layer,
      ),
    )
