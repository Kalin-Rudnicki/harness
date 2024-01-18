package harness.payments.facades

import scala.scalajs.js

final class ConfirmSetupOptions(
    val elements: Elements,
    val clientSecret: String,
    val confirmParams: ConfirmParams
) extends js.Object
