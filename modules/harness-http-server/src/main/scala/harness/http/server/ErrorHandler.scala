package harness.http.server

import harness.http.server.error.*
import harness.web.{ErrorCodec, HttpCode}
import harness.zio.{ErrorLogger, ErrorMapper}

final case class ErrorHandler[DomainError, ApiError](
    convertDecodingFailure: DecodingFailure => DomainError,
    convertUnexpectedError: Throwable => DomainError,
    errorCode: ApiError => HttpCode,
    errorCodec: ErrorCodec[ApiError],
    errorConverter: ErrorMapper[DomainError, ApiError],
    errorLogger: ErrorLogger[DomainError],
)
