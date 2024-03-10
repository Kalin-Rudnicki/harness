package template.domain.impl.storage

import harness.zio.test.*
import template.domain.impl.storage.inMemory.*
import template.domain.storage.*
import zio.*

object InMemoryUserStorageSpec
    extends DefaultHarnessSpec.ForContract[UserStorage]("InMemoryUserStorage", UserStorageContract)(
      ZLayer.make[UserStorage](
        DbState.layer,
        InMemoryUserStorage.layer,
      ),
    )
