package harness.http.server.error

final case class DecodingFailure(error: String) extends Throwable {
  override def getMessage: String = s"Decoding failure: $error"
}
