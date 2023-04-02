package harness.web.server

import harness.core.HError
import harness.web.HttpCode

final case class ErrorWithHTTPCode(httpCode: HttpCode, child: HError) extends HError.Single(child.userMessage, child.internalMessage, child.causes)
