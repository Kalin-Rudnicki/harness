package template.api.impl

import harness.endpoint.error.DecodingFailure
import harness.endpoint.spec.SchemaSource
import harness.http.server.ErrorHandler
import harness.zio.ErrorLogger
import template.api.model.error.ApiError
import template.domain.model.DomainError

implicit val errorHandler: ErrorHandler.Id[DomainError, ApiError] =
  ErrorHandler.id[DomainError, ApiError](
    convertDecodingFailure = {
      case DecodingFailure.Simple(SchemaSource.HeaderOrCookie(key) :: Nil, DecodingFailure.Cause.MissingRequired) if key.contains("HARNESS-WEB-APP-TEMPLATE-SESSION--") =>
        DomainError.MissingSessionToken
      case e => DomainError.FailedToDecodeInput(e.getMessage)
    },
    convertUnexpectedError = DomainError.UnexpectedServerError(_),
    convertDomainError = _.toApi,
    errorLogger = ErrorLogger.jsonEncoded[DomainError].atLevel.error, // TODO (KR) :
    headersAndCookiesOnError = {
      case ApiError.InvalidSessionToken => identity // TODO (KR) : need token key - unset
      case _                            => identity
    },
  )
