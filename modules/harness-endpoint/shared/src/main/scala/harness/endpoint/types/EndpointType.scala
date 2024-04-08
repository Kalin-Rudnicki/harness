package harness.endpoint.types

// TODO (KR) : Add ability to encode/decode values to response headers

/**
  * @tparam InputWithCookies Inputs which come from path + query + headers
  * @tparam InputWithoutCookies Inputs which come from path + query + headers + cookies
  * @tparam InputBody Type of input which comes from request body
  * @tparam OutputBody Type of output which is written to response body
  * @tparam Error Error type of the endpoint
  */
final class EndpointType[InputWithCookies, InputWithoutCookies, InputBody <: BodyType, OutputBody <: BodyType, Error] private ()
object EndpointType {
  type Any = EndpointType[?, ?, ? <: BodyType, ? <: BodyType, ?]
  type Basic[Input, InputBody <: BodyType, OutputBody <: BodyType, Error] = EndpointType[Input, Input, InputBody, OutputBody, Error]
}
