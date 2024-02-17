package harness.http.server.error

final case class DecodingFailure(error: String) extends Throwable
