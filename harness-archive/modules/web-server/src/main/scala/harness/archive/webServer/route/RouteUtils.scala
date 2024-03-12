package harness.archive.webServer.route

import harness.archive.api.model.error.ApiError
import harness.archive.api.model as Api
import harness.archive.domain.model.DomainError
import harness.archive.webServer.api.SessionConfig
import harness.http.server.*
import harness.http.server.error.DecodingFailure
import harness.web.*
import harness.zio.*
import zio.*

object RouteUtils {

  implicit val errorHandler: ErrorHandler[DomainError, ApiError] =
    ErrorHandler[DomainError, ApiError](
      convertDecodingFailure = _.toDomain,
      convertUnexpectedError = DomainError.UnexpectedServerError(_),
      errorCode = {
        case ApiError.MissingSessionToken        => HttpCode.`401`
        case ApiError.InvalidSessionToken        => HttpCode.`401`
        case ApiError.MissingAppToken            => HttpCode.`401`
        case ApiError.InvalidAppToken            => HttpCode.`401`
        case ApiError.AppNotFound                => HttpCode.`404`
        case ApiError.UsernameAlreadyExists      => HttpCode.`400`
        case ApiError.AppNameAlreadyExists       => HttpCode.`400`
        case ApiError.InvalidLoginCredentials    => HttpCode.`403`
        case ApiError.UserDoesNotHaveAccessToApp => HttpCode.`403`
        case ApiError.InvalidInput(_)            => HttpCode.`400`
        case ApiError.InternalServerError        => HttpCode.`500`
      },
      errorCodec = ApiError.errorCodec,
      errorConverter = _.toApi,
      errorLogger = ErrorLogger.withJsonPrettyShow[DomainError].withLevel {
        case DomainError.MissingSessionToken              => Logger.LogLevel.Warning
        case DomainError.InvalidSessionToken              => Logger.LogLevel.Warning
        case DomainError.MissingAppToken                  => Logger.LogLevel.Warning
        case DomainError.InvalidAppToken                  => Logger.LogLevel.Warning
        case DomainError.AppNotFound(_, _)                => Logger.LogLevel.Info
        case DomainError.UsernameAlreadyExists(_)         => Logger.LogLevel.Warning
        case DomainError.AppNameAlreadyExists(_, _)       => Logger.LogLevel.Info
        case DomainError.InvalidUsername(_)               => Logger.LogLevel.Warning
        case DomainError.InvalidPassword                  => Logger.LogLevel.Warning
        case DomainError.UserDoesNotHaveAccessToApp(_, _) => Logger.LogLevel.Warning
        case DomainError.FailedToSendEmail(_)             => Logger.LogLevel.Error
        case DomainError.UnexpectedStorageError(_)        => Logger.LogLevel.Error
        case DomainError.UnexpectedServerError(_)         => Logger.LogLevel.Error
        case DomainError.UnexpectedPaymentError(_)        => Logger.LogLevel.Error
        case DomainError.MissingExpectedInStorage(_)      => Logger.LogLevel.Error
        case DomainError.FailedToDecodeInput(_)           => Logger.LogLevel.Info
      },
    )

  implicit class DecodingFailureOps(self: DecodingFailure) {
    def toDomain: DomainError =
      DomainError.FailedToDecodeInput(self.error)
  }

  val sessionTokenOptional: ZIO[HttpRequest & SessionConfig, DomainError, Option[Api.user.UserToken]] =
    ZIO.serviceWithZIO[SessionConfig] { sessionConfig =>
      HttpRequest.header.find[Api.user.UserToken](sessionConfig.key).mapError(_.toDomain).someOrElseZIOOpt {
        HttpRequest.cookie.find[Api.user.UserToken](sessionConfig.key).mapError(_.toDomain)
      }
    }

  val sessionToken: ZIO[HttpRequest & SessionConfig, DomainError, Api.user.UserToken] =
    sessionTokenOptional.someOrFail(DomainError.MissingSessionToken)

}
