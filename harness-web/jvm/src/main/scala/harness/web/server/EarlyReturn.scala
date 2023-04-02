package harness.web.server

import harness.core.HError

final case class EarlyReturn(response: HttpResponse) extends HError.Single(HError.UserMessage.Const("EarlyReturn: Not an error"), "EarlyReturn: Not an error", Nil)
