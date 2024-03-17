package harness.http.client.error

final case class HeaderError(header: String, msg: String) extends Throwable {
  override def getMessage: String = s"Error with header '$header': $msg"
}
