package harness.http.server

import harness.core.HError
import harness.web.HttpCode

final case class ErrorWithHTTPCode(httpCode: HttpCode, child: HError) extends HError.Single(child.userMessage, child.internalMessage, child.causes)

extension (hError: HError) {
  def withHTTPCode(httpCode: HttpCode): HError = ErrorWithHTTPCode(httpCode, hError)
}
