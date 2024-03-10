package harness.archive.domain.impl.storage

import harness.zio.test.*
import harness.archive.domain.impl.storage.inMemory.*
import harness.archive.domain.storage.*
import zio.*

object InMemoryPaymentMethodStorageSpec
    extends DefaultHarnessSpec.ForContract[UserStorage & PaymentMethodStorage]("InMemoryPaymentMethodStorage", PaymentMethodStorageContract)(
      ZLayer.make[UserStorage & PaymentMethodStorage](
        DbState.layer,
        InMemoryUserStorage.layer,
        InMemoryPaymentMethodStorage.layer,
      ),
    )
