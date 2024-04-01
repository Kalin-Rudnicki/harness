package harness.http.server

import harness.endpoint.error.DecodingFailure
import harness.endpoint.spec.*
import harness.endpoint.spec.NonBodyInputCodec.Result
import harness.endpoint.transfer.*
import harness.endpoint.typeclass.*
import harness.endpoint.types.*
import harness.endpoint.types.Types.*
import harness.zio.*
import zio.*

final case class Endpoint[-R, ET <: EndpointType.Any](
    spec: EndpointSpec[ET],
    implementation: Implementation[R, ET],
) {

  private val errorHandler = implementation.errorHandler

  def handle(
      request: HttpRequest,
      parsedPath: spec.inputWithCookiesCodec.PathT,
  ): ZIO[HarnessEnv & R, implementation.DomainError, HttpResponse[Send[OutputBody[ET]]]] =
    (for {
      inputWithCookies <-
        (spec.inputWithCookiesCodec.decodeAll(
          parsedPath,
          request.queries,
          request.headers,
          request.cookies,
        ) match {
          case Result.Success(inputWithCookies) => ZIO.succeed(inputWithCookies)
          case fail: Result.Fail                => ZIO.fail(DecodingFailure.fromFailure(fail))
        }).mapError(errorHandler.convertDecodingFailure)
      body <-
        spec.inputBodyCodec
          .in(request.contentLengths, request.rawInputStream)
          .mapError(errorHandler.convertDecodingFailure)

      result <- implementation.impl(inputWithCookies, body)
    } yield result)
      .provideSomeEnvironment[HarnessEnv & R](_ ++ ZEnvironment(request))

  def handleRaw(
      request: HttpRequest,
      parsedPath: spec.inputWithCookiesCodec.PathT,
  ): ZIO[HarnessEnv & R, implementation.DomainError, HttpResponse[OutputStream]] =
    handle(request, parsedPath).map { result => result.copy(body = spec.outputBodyCodec.out(result.body)) }

  def parseAndHandle(
      request: HttpRequest,
  ): ZIO[HarnessEnv & R, implementation.DomainError, Option[HttpResponse[Send[OutputBody[ET]]]]] =
    spec.inputWithCookiesCodec.decodePath(request.path) match {
      case Some(parsedPath) => handle(request, parsedPath).asSome
      case None             => ZIO.none
    }

}
object Endpoint {

  type Projection[R] = [ET <: EndpointType.Any] =>> Endpoint[R, ET]

  def make[R, T[_[_ <: EndpointType.Any]]](
      spec: T[EndpointSpec],
      impl: T[Implementation.Projection[R]],
  )(using
      zip: Zipper[T],
      mapK: MapK[T],
  ): T[Endpoint.Projection[R]] =
    MapK.mapK(Zipper.zip(spec, impl)) { [t <: EndpointType.Any] => (a: K11ET.Zip[EndpointSpec, Implementation.Projection[R]][t]) => Endpoint(a._1, a._2) }

}
