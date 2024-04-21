package template.api.impl

import harness.http.server.ErrorHandler
import template.api.model.error.ApiError
import template.domain.model.DomainError

implicit val errorHandler: ErrorHandler.Id[DomainError, ApiError] =
  ErrorHandler.id[DomainError, ApiError](
    convertDecodingFailure = ???, // TODO (KR) :
    convertUnexpectedError = ???, // TODO (KR) :
    convertDomainError = ???, // TODO (KR) :
    errorLogger = ???, // TODO (KR) :
    headersAndCookiesOnError = ???, // TODO (KR) :
  )
