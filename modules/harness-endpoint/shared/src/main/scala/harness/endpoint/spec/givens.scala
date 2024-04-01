package harness.endpoint.spec

import harness.endpoint.types.{BodyType, EndpointType}

given [Path, AllWithCookies, AllWithoutCookies, InputBody <: BodyType, OutputBody <: BodyType]: Conversion[
  EndpointSpec.Builder5[Path, AllWithCookies, AllWithoutCookies, InputBody, OutputBody],
  EndpointSpec[EndpointType[AllWithCookies, AllWithoutCookies, InputBody, OutputBody, Nothing]],
] =
  _ /!--> errorBody.none
