package harness.archive.api.model.error

import harness.web.ErrorCodec
import zio.json.*

sealed trait ApiError
object ApiError {

  case object MissingSessionToken extends ApiError
  case object InvalidSessionToken extends ApiError
  case object UsernameAlreadyExists extends ApiError
  case object InvalidLoginCredentials extends ApiError
  case object EmailNotVerified extends ApiError
  case object EmailAlreadyVerified extends ApiError
  case object InvalidEmailVerificationCode extends ApiError
  case object InternalServerError extends ApiError
  final case class InvalidInput(error: String) extends ApiError

  implicit val jsonCodec: JsonCodec[ApiError] = DeriveJsonCodec.gen

  implicit val errorCodec: ErrorCodec[ApiError] = ErrorCodec.jsonCodec[ApiError]

}
