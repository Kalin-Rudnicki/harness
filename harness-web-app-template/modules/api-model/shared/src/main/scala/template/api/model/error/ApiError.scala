package template.api.model.error

import harness.endpoint.spec.*
import harness.web.HttpCode
import zio.json.*

@jsonDiscriminator("type")
sealed trait ApiError
object ApiError {

  @errorCode(HttpCode.Unauthorized)
  @errorExamples(MissingSessionToken)
  case object MissingSessionToken extends ApiError

  @errorCode(HttpCode.Unauthorized)
  @errorExamples(InvalidSessionToken)
  case object InvalidSessionToken extends ApiError

  @errorCode(HttpCode.Conflict)
  @errorExamples(UsernameAlreadyExists)
  case object UsernameAlreadyExists extends ApiError

  @errorCode(HttpCode.BadRequest)
  @errorExamples(InvalidLoginCredentials)
  case object InvalidLoginCredentials extends ApiError

  @errorCode(HttpCode.Forbidden)
  @errorExamples(EmailNotVerified)
  case object EmailNotVerified extends ApiError

  @errorCode(HttpCode.BadRequest)
  @errorExamples(EmailAlreadyVerified)
  case object EmailAlreadyVerified extends ApiError

  @errorCode(HttpCode.BadRequest)
  @errorExamples(InvalidEmailVerificationCode)
  case object InvalidEmailVerificationCode extends ApiError

  @errorCode(HttpCode.InternalServerError)
  @errorExamples(InternalServerError)
  case object InternalServerError extends ApiError

  @errorCode(HttpCode.BadRequest)
  @errorExamples(InvalidInput("error message"))
  final case class InvalidInput(error: String) extends ApiError

  implicit val schema: ErrorSchema[ApiError] = ErrorSchema.derive

}
