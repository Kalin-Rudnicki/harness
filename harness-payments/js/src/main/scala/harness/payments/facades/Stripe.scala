package harness.payments.facades

import scala.annotation.unused
import scalajs.js
import scalajs.js.annotation.*

@js.native
@JSGlobal
final class Stripe(@unused apiKey: String) extends js.Any {
  def elements(options: ElementsOptions): Elements = js.native
  def confirmSetup(confirmSetupOptions: ConfirmSetupOptions): js.Promise[MaybeErrorResponse] = js.native
}
