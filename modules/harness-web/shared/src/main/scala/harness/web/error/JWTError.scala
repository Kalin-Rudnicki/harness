package harness.web.error

sealed trait JWTError {

  // TODO (KR) :

}
object JWTError {
  final case class UnableToDecodeBody(message: String) extends JWTError
  case object InvalidSignature extends JWTError
  case object ExpiredToken extends JWTError
}
