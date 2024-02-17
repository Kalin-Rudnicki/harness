package harness.payments.facades

import scalajs.js

@js.native
trait Elements extends js.Object {
  def create(str: String): PaymentElement
  def submit(): js.Promise[MaybeErrorResponse]
}
