package harness.payments.facades

import scala.scalajs.js

@js.native
trait PaymentElement extends js.Object {
  def mount(str: String): js.Any
}
