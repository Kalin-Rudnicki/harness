package harness.web.error

final case class UnexpectedResponseBody(body: String, error: String) extends Throwable
