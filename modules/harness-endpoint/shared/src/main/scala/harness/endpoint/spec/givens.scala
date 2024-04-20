package harness.endpoint.spec

import harness.endpoint.error.ApiInternalDefect
import harness.endpoint.types.{BodyType, EndpointType}

implicit def convertSpecBuilder[Path, AllWithCookies, AllWithoutCookies, InputBody <: BodyType, OutputBody <: BodyType]: Conversion[
  EndpointSpec.Builder5[Path, AllWithCookies, AllWithoutCookies, InputBody, OutputBody],
  EndpointSpec[EndpointType[AllWithCookies, AllWithoutCookies, InputBody, OutputBody, ApiInternalDefect]],
] =
  _ /!--> errorBody.json[ApiInternalDefect]
