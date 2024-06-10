package harness.endpoint.spec

import harness.endpoint.types.*

implicit def convertSpecBuilder[PathT, QueryT, AuthT, HeaderT, InputBodyT <: BodyType, OutputBodyT <: BodyType, ErrorT]: Conversion[
  EndpointSpec.Builder7[PathT, QueryT, AuthT, HeaderT, InputBodyT, OutputBodyT, ErrorT],
  EndpointSpec[EndpointType[PathT, QueryT, AuthT, HeaderT, InputBodyT, OutputBodyT, ErrorT]],
] =
  _.toEndpointSpec
