package template.api.impl

import harness.endpoint.error.DecodingFailure
import harness.http.server.ErrorHandler
import harness.zio.ErrorLogger
import template.api.model.error.ApiError
import template.domain.model.DomainError

implicit val errorHandler: ErrorHandler.Id[DomainError, ApiError] =
  ErrorHandler.id[DomainError, ApiError](
    convertDecodingFailure = {
      case DecodingFailure(DecodingFailure.Source.HeaderOrCookie(key), _) if key.contains("HARNESS-WEB-APP-TEMPLATE-SESSION--") =>
        DomainError.MissingSessionToken
      case e => DomainError.FailedToDecodeInput(e.getMessage)
    },
    convertUnexpectedError = DomainError.UnexpectedServerError(_),
    convertDomainError = _.toApi,
    errorLogger = ErrorLogger.withJsonShow[DomainError].atLevel.error, // TODO (KR) :
    headersAndCookiesOnError = {
      case ApiError.InvalidSessionToken => identity // TODO (KR) : need token key - unset
      case _                            => identity
    },
  )
