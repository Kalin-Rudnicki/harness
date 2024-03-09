package template.domain.impl.storage

import harness.zio.test.*
import template.domain.impl.storage.inMemory.*
import template.domain.storage.*
import zio.*

object InMemoryPaymentMethodStorageSpec
    extends DefaultHarnessSpec.ForContract[UserStorage & PaymentMethodStorage]("InMemoryPaymentMethodStorage", PaymentMethodStorageContract)(
      ZLayer.make[UserStorage & PaymentMethodStorage](
        DbState.layer,
        InMemoryUserStorage.layer,
        InMemoryPaymentMethodStorage.layer,
      ),
    )
