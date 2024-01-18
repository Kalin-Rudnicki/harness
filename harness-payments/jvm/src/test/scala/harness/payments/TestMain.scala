package harness.payments

import cats.syntax.option.*
import harness.email.*
import harness.zio.*
import java.util.UUID
import zio.*

object TestMain extends ExecutableApp {

  override val executable: Executable =
    Executable
      .withLayer {
        ZLayer.succeed(PaymentProcessor.StripePaymentProcessor.Config("sk_test_51OVNiaD9gOL4yaWVWoQKP06HKSQ0kvbzT8AT4Bc9CmPx77OqA0jwwBjea0VxLIvt5UNLZSNiT8qlATdVhAr9URzm00nLuElEbN")) >>>
          PaymentProcessor.StripePaymentProcessor.layer
      }
      .withEffect {
        for {
          _ <- Logger.log.info("payments.TestMain")
          paymentToken = PaymentSourceId("tok_visa")
          _ <- Logger.log.info(s"paymentToken: $paymentToken")
          payment = Charge(
            amountInCents = 1234L,
            currency = Currency.USD,
            description = s"Test Charge : ${UUID.randomUUID}",
            source = paymentToken,
            email = EmailAddress.parseUnsafe("kalin.rudnicki@gmail.com").some,
          )
          chargeId <- PaymentProcessor.processCharge(payment)
          _ <- Logger.log.info(s"chargeId: $chargeId")
        } yield ()
      }

}
