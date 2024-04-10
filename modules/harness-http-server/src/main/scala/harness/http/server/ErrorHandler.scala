package harness.http.server

import harness.endpoint.error.DecodingFailure
import harness.zio.{ErrorLogger, ErrorMapper}

final case class ErrorHandler[DomainError, ApiError](
    convertDecodingFailure: DecodingFailure => DomainError,
    convertUnexpectedError: Throwable => DomainError,
    errorConverter: ErrorMapper[DomainError, ApiError],
    errorLogger: ErrorLogger[DomainError],
    headersAndCookiesOnError: ApiError => HttpResponse[OutputResult] => HttpResponse[OutputResult],
)
