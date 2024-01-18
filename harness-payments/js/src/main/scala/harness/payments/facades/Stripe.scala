package harness.payments.facades

import scalajs.js
import scalajs.js.annotation.*

@js.native
@JSGlobal
final class Stripe(apiKey: String) extends js.Any {
  def elements(options: ElementsOptions): Elements = js.native
  def confirmSetup(confirmSetupOptions: ConfirmSetupOptions): js.Promise[MaybeErrorResponse] = js.native
}
