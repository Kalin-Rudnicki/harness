package harness.endpoint.types

import cats.data.NonEmptyList
import harness.endpoint.error.ApiInternalDefect

// TODO (KR) : Add ability to encode/decode values to response headers

/**
  * @tparam Path Inputs which come from path
  * @tparam Query Inputs which come from query params
  * @tparam Auth Inputs which come from headers, and are used for auth
  * @tparam Header Inputs which come from headers
  * @tparam InputBody Type of input which comes from request body
  * @tparam OutputBody Type of output which is written to response body
  * @tparam Error Error type of the endpoint
  */
final class EndpointType[Path, Query, Auth, Header, InputBody <: BodyType, OutputBody <: BodyType, Error] private ()
object EndpointType {

  type Any = EndpointType[?, ?, ?, ?, ? <: BodyType, ? <: BodyType, ?]

  trait Builder7[Path, Query, Auth, Header, InputBody <: BodyType, OutputBody <: BodyType, Error] {
    type Build = EndpointType[Path, Query, Auth, Header, InputBody, OutputBody, Error]
  }

  trait Builder6[Path, Query, Auth, Header, InputBody <: BodyType, OutputBody <: BodyType] extends Builder7[Path, Query, Auth, Header, InputBody, OutputBody, ApiInternalDefect] {
    type Error[E] = Builder7[Path, Query, Auth, Header, InputBody, OutputBody, E]
  }

  trait Builder5[Path, Query, Auth, Header, InputBody <: BodyType] extends Builder6[Path, Query, Auth, Header, InputBody, BodyType.None] {
    type OutputBody[O <: BodyType] = Builder6[Path, Query, Auth, Header, InputBody, O]
    type OutputBodyEncoded[I] = OutputBody[BodyType.Encoded[I]]
    type OutputBodyStream = OutputBody[BodyType.Stream]
  }

  trait Builder4[Path, Query, Auth, Header] extends Builder5[Path, Query, Auth, Header, BodyType.None] {
    type InputBody[I <: BodyType] = Builder5[Path, Query, Auth, Header, I]
    type InputBodyEncoded[I] = InputBody[BodyType.Encoded[I]]
    type InputBodyStream = InputBody[BodyType.Stream]
  }

  trait Builder3[Path, Query, Auth] extends Builder4[Path, Query, Auth, Unit] {
    type Header[H] = Builder4[Path, Query, Auth, H]
  }

  trait Builder2[Path, Query] extends Builder3[Path, Query, Unit] {
    type Auth[A] = Builder3[Path, Query, A]
  }

  trait Builder1[Path] extends Builder2[Path, Unit] {
    type Query[Q] = Builder2[Path, Q]
  }

  trait Builder extends Builder1[Unit] {
    type Path[P] = Builder1[P]
    type PathMany = Path[List[String]]
    type PathManyNonEmpty = Path[NonEmptyList[String]]
  }

}
