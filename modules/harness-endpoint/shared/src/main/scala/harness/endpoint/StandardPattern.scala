package harness.endpoint

import harness.endpoint.spec.*
import harness.endpoint.typeclass.*
import harness.endpoint.types.*
import harness.schema.*
import harness.web.HttpCode
import zio.json.*

final case class StandardPattern[T[_[_ <: EndpointType.Any]], F[_ <: EndpointType.Any]](
    api: T[F],
    healthCheck: F[StandardPattern.HealthCheck],
    index: F[StandardPattern.Index],
    page: F[StandardPattern.Page],
    favicon: F[StandardPattern.Favicon],
    js: F[StandardPattern.Js],
)
object StandardPattern {

  type Projection[T[_[_ <: EndpointType.Any]]] = [F[_ <: EndpointType.Any]] =>> StandardPattern[T, F]

  @jsonDiscriminator("type")
  sealed trait ApiError
  object ApiError {

    @errorCode(HttpCode.`404`)
    @errorExamples(NotFound)
    case object NotFound extends ApiError

    @errorCode(HttpCode.`500`)
    @errorExamples(InternalDefect)
    case object InternalDefect extends ApiError

    implicit val errorSchema: ErrorSchema[ApiError] = ErrorSchema.derived

  }

  type HealthCheck = EndpointType.Builder#Build
  type Index = EndpointType.Builder#Build
  type Page = EndpointType.Builder#PathMany#OutputBodyEncoded[String]#Build
  type Favicon = EndpointType.Builder#OutputBodyStream#Error[ApiError]#Build
  type Js = EndpointType.Builder#PathMany#OutputBodyStream#Error[ApiError]#Build

  // TODO (KR) : improve descriptions
  def spec[T[_[_ <: EndpointType.Any]]](
      tSpec: T[EndpointSpec],
  )(implicit pp: PathPrepend[T]): StandardPattern[T, EndpointSpec] =
    StandardPattern[T, EndpointSpec](
      api = "api" /: tSpec,
      healthCheck = (EndpointSpec.get("Health Check") / "health-check")
        .describe("Check to see if the web server is healthy")
        .internal,
      index = EndpointSpec
        .get("Index")
        .describe("Redirects to /page")
        .hidden,
      page = (EndpointSpec.get("Page") / "page" / pathRest("page-path") /--> body.raw[String])
        .describe("Web UI"),
      favicon = (EndpointSpec.get("Page") / "res" / "favicon.ico" /--> body.stream /!--> errorBody.json[ApiError])
        .describe("Icon for website")
        .hidden,
      js = (EndpointSpec.get("Javascript Resource") / "res" / "js" / pathRest("res-path") /--> body.stream /!--> errorBody.json[ApiError])
        .describe("Loads javascript for SPA")
        .hidden,
    )

}
