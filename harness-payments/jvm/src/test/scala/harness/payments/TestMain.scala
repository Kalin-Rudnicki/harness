package harness.payments

import cats.syntax.option.*
import harness.email.*
import harness.payments.service.PaymentProcessor
import harness.zio.*
import java.util.UUID
import zio.*

object TestMain extends ExecutableApp {

  override val executable: Executable =
    Executable
      .withLayer {
        ZLayer.succeed(
          PaymentProcessor.StripePaymentProcessor.Config(
            "pk_test_51OVNiaD9gOL4yaWVMKdhPEOW59IeVbst1031HrqDDQRswYNFYAQtiOg9UDSyST7DYLGq8CYVN0bG0q51GovrVpVz0070Gb4ccu",
            "sk_test_51OVNiaD9gOL4yaWVWoQKP06HKSQ0kvbzT8AT4Bc9CmPx77OqA0jwwBjea0VxLIvt5UNLZSNiT8qlATdVhAr9URzm00nLuElEbN",
          ),
        ) >>>
          PaymentProcessor.StripePaymentProcessor.layer
      }
      .withEffect {
        for {
          _ <- Logger.log.info("payments.TestMain")
        } yield ()
      }

}
