package harness.http.server

import harness.endpoint.error.{ApiInternalDefect, DecodingFailure}
import harness.zio.{ErrorLogger, ErrorMapper}

trait ErrorHandler[DomainError, ApiError] {

  type Err
  final type _DomainError = DomainError

  val convertDecodingFailure: DecodingFailure => DomainError
  val convertUnexpectedError: Throwable => DomainError
  val convertErr: ErrorMapper[Err, DomainError]
  val convertDomainError: ErrorMapper[DomainError, ApiError]
  val errorLogger: ErrorLogger[DomainError]
  val headersAndCookiesOnError: ApiError => HttpResponse[OutputResult] => HttpResponse[OutputResult]

}
object ErrorHandler {

  type Id[DomainError, ApiError] = ErrorHandler.Aux[DomainError, DomainError, ApiError]
  type Aux[_Err, DomainError, ApiError] = ErrorHandler[DomainError, ApiError] { type Err = _Err }

  def apply[_Err, DomainError, ApiError](
      _convertDecodingFailure: DecodingFailure => DomainError,
      _convertUnexpectedError: Throwable => DomainError,
      _convertErr: ErrorMapper[_Err, DomainError],
      _convertDomainError: ErrorMapper[DomainError, ApiError],
      _errorLogger: ErrorLogger[DomainError],
      _headersAndCookiesOnError: ApiError => HttpResponse[OutputResult] => HttpResponse[OutputResult],
  ): ErrorHandler.Aux[_Err, DomainError, ApiError] =
    new ErrorHandler[DomainError, ApiError] {
      override type Err = _Err
      override val convertDecodingFailure: DecodingFailure => DomainError = _convertDecodingFailure
      override val convertUnexpectedError: Throwable => DomainError = _convertUnexpectedError
      override val convertErr: ErrorMapper[Err, DomainError] = _convertErr
      override val convertDomainError: ErrorMapper[DomainError, ApiError] = _convertDomainError
      override val errorLogger: ErrorLogger[DomainError] = _errorLogger
      override val headersAndCookiesOnError: ApiError => HttpResponse[OutputResult] => HttpResponse[OutputResult] = _headersAndCookiesOnError
    }

  def id[DomainError, ApiError](
      convertDecodingFailure: DecodingFailure => DomainError,
      convertUnexpectedError: Throwable => DomainError,
      convertDomainError: ErrorMapper[DomainError, ApiError],
      errorLogger: ErrorLogger[DomainError],
      headersAndCookiesOnError: ApiError => HttpResponse[OutputResult] => HttpResponse[OutputResult],
  ): ErrorHandler.Aux[DomainError, DomainError, ApiError] =
    ErrorHandler[DomainError, DomainError, ApiError](
      convertDecodingFailure,
      convertUnexpectedError,
      ErrorMapper.id,
      convertDomainError,
      errorLogger,
      headersAndCookiesOnError,
    )

  implicit def apiInternalDefect: ErrorHandler.Aux[Nothing, Throwable, ApiInternalDefect] =
    ErrorHandler[Nothing, Throwable, ApiInternalDefect](
      identity,
      identity,
      _ => ???,
      _ => ApiInternalDefect.InternalDefect,
      ErrorLogger.withGetMessage[Throwable].atLevel.error,
      _ => identity,
    )

}
