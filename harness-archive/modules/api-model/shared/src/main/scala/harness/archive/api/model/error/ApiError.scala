package harness.archive.api.model.error

import harness.web.ErrorCodec
import zio.json.*

sealed trait ApiError
object ApiError {

  case object MissingSessionToken extends ApiError
  case object InvalidSessionToken extends ApiError
  case object MissingAppToken extends ApiError
  case object InvalidAppToken extends ApiError
  case object AppNotFound extends ApiError
  case object UsernameAlreadyExists extends ApiError
  case object AppNameAlreadyExists extends ApiError
  case object InvalidLoginCredentials extends ApiError
  case object InternalServerError extends ApiError
  final case class InvalidInput(error: String) extends ApiError
  case object UserDoesNotHaveAccessToApp extends ApiError

  implicit val jsonCodec: JsonCodec[ApiError] = DeriveJsonCodec.gen

  implicit val errorCodec: ErrorCodec[ApiError] = ErrorCodec.jsonCodec[ApiError]

}
