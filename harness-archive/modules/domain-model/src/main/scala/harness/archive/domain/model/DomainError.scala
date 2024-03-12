package harness.archive.domain.model

import cats.data.NonEmptyList
import harness.archive.api.model.error.ApiError
import harness.archive.api.model as Api
import harness.zio.ZIOJsonInstances.*
import zio.json.*

sealed trait DomainError {

  final def toApi: ApiError =
    this match {
      // =====| Core |=====
      case DomainError.MissingSessionToken              => ApiError.MissingSessionToken
      case DomainError.InvalidSessionToken              => ApiError.InvalidSessionToken
      case DomainError.MissingAppToken                  => ApiError.MissingAppToken
      case DomainError.InvalidAppToken                  => ApiError.InvalidAppToken
      case DomainError.AppNotFound(_, _)                  => ApiError.AppNotFound
      case DomainError.UsernameAlreadyExists(_)         => ApiError.UsernameAlreadyExists
      case DomainError.AppNameAlreadyExists(_, _)       => ApiError.AppNameAlreadyExists
      case DomainError.InvalidUsername(_)               => ApiError.InvalidLoginCredentials
      case DomainError.InvalidPassword                  => ApiError.InvalidLoginCredentials
      case DomainError.UserDoesNotHaveAccessToApp(_, _) => ApiError.UserDoesNotHaveAccessToApp
      // =====| Misc |=====
      case DomainError.FailedToSendEmail(_)        => ApiError.InternalServerError
      case DomainError.UnexpectedStorageError(_)   => ApiError.InternalServerError
      case DomainError.UnexpectedServerError(_)    => ApiError.InternalServerError
      case DomainError.UnexpectedPaymentError(_)   => ApiError.InternalServerError
      case DomainError.MissingExpectedInStorage(_) => ApiError.InternalServerError
      case DomainError.FailedToDecodeInput(error)  => ApiError.InvalidInput(error)
    }

}
object DomainError {

  // =====| Core |=====

  case object MissingSessionToken extends DomainError

  case object InvalidSessionToken extends DomainError

  case object MissingAppToken extends DomainError

  case object InvalidAppToken extends DomainError

  final case class AppNotFound(userId: Api.user.UserId, appName: String) extends DomainError

  final case class UsernameAlreadyExists(username: String) extends DomainError

  final case class AppNameAlreadyExists(userId: Api.user.UserId, appName: String) extends DomainError

  final case class InvalidUsername(username: String) extends DomainError

  case object InvalidPassword extends DomainError

  final case class UserDoesNotHaveAccessToApp(userId: Api.user.UserId, appId: Api.app.AppId) extends DomainError

  // =====| Misc |=====

  final case class FailedToSendEmail(cause: Throwable) extends DomainError

  final case class UnexpectedStorageError(cause: Throwable) extends DomainError
  object UnexpectedStorageError {
    def fromMessage(causeMessage: String): UnexpectedStorageError = UnexpectedStorageError(new RuntimeException(causeMessage))
  }

  final case class UnexpectedServerError(cause: Throwable) extends DomainError

  final case class UnexpectedPaymentError(cause: Throwable) extends DomainError

  final case class MissingExpectedInStorage(keys: NonEmptyList[String]) extends DomainError
  object MissingExpectedInStorage {
    def apply(key0: String, keyN: String*): MissingExpectedInStorage = MissingExpectedInStorage(NonEmptyList(key0, keyN.toList))
  }

  final case class FailedToDecodeInput(error: String) extends DomainError

  implicit val jsonCodec: JsonCodec[DomainError] = DeriveJsonCodec.gen

}
