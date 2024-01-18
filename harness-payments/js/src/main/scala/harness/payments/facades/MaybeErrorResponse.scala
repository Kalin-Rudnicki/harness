package harness.payments.facades

import scala.scalajs.js

@js.native
trait MaybeErrorResponse extends js.Object {
  def error: js.UndefOr[Err]
}
