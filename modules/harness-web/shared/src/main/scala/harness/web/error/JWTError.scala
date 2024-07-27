package harness.web.error

sealed trait JWTError {

  override def toString: String = this match
    case JWTError.UnableToDecodePayload(message) => s"Unable to decode JWT payload: $message"
    case JWTError.InvalidSignature               => "JWT has invalid signature"
    case JWTError.ExpiredToken                   => "JWT is expired"

}
object JWTError {
  final case class UnableToDecodePayload(message: String) extends JWTError
  case object InvalidSignature extends JWTError
  case object ExpiredToken extends JWTError
}
