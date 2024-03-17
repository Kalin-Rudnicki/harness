package harness.payments

import harness.payments.service.PaymentProcessor
import harness.zio.*
import zio.*

object TestMain extends ExecutableApp {

  override val executable: Executable =
    Executable
      .withLayer {
        HConfig.readLayer[PaymentProcessor.StripePaymentProcessor.Config]("payment", "stripe") >>>
          PaymentProcessor.StripePaymentProcessor.layer
      }
      .withThrowableEffect {
        for {
          _ <- Logger.log.info("payments.TestMain")
        } yield ()
      }

}
