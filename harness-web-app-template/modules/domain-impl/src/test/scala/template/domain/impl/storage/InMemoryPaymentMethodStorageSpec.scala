package template.domain.impl.storage

import harness.zio.test.*
import template.domain.impl.storage.inMemory.*
import template.domain.storage.*
import zio.*

object InMemoryPaymentMethodStorageSpec extends ContractHarnessSpec[UserStorage & PaymentMethodStorage]("InMemoryPaymentMethodStorage", PaymentMethodStorageContract) {

  override def layerProvider: LayerProvider[R] =
    LayerProvider
      .providePerTest(
        DbState.layer,
        InMemoryUserStorage.layer,
        InMemoryPaymentMethodStorage.layer,
      )

}
