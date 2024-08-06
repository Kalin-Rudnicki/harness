package harness.http.server

import harness.endpoint.error.DecodingFailure
import harness.endpoint.spec.*
import harness.endpoint.transfer.*
import harness.endpoint.typeclass.*
import harness.endpoint.types.*
import harness.endpoint.types.Types.*
import harness.zio.{Path as _, *}
import zio.*

final case class Endpoint[-R, ET <: EndpointType.Any](
    spec: EndpointSpec[ET],
    implementation: Implementation[R, ET],
) {

  private val errorHandler = implementation.errorHandler

  private def decode(request: HttpRequest): IO[DecodingFailure, (Query[ET], Auth[ET], Header[ET], Receive[InputBody[ET]])] =
    for {
      q <- ZIO.fromEither { spec.queryCodec.decode(request.queries) }
      a <- ZIO.fromEither { spec.authHeaderCodec.decode(request.headers, request.cookies) }
      h <- ZIO.fromEither { spec.headerCodec.decode(request.headers, request.cookies) }
      b <- spec.inputBodyCodec.in(request.contentLengths, request.rawInputStream)
    } yield (q, a, h, b)

  def handleScoped(
      request: HttpRequest,
      parsedPath: Path[ET],
  ): ZIO[Scope & R, implementation.DomainError, HttpResponse[Send[OutputBody[ET]]]] =
    Logger.log.debug(s"Headers:${request.headers.keySet.toSeq.sorted.map(h => s"\n  - $h").mkString}") *>
      decode(request)
        .mapError(errorHandler.convertDecodingFailure)
        .flatMap { implementation.impl(parsedPath, _, _, _, _) }
        .provideSomeEnvironment[Scope & R](_ ++ ZEnvironment(request))

  def handle(
      request: HttpRequest,
      parsedPath: Path[ET],
  ): ZIO[R, implementation.DomainError, HttpResponse[Send[OutputBody[ET]]]] =
    ZIO.scoped { handleScoped(request, parsedPath) }

  def handleRaw(
      request: HttpRequest,
      parsedPath: Path[ET],
  ): ZIO[R, implementation.DomainError, HttpResponse[OutputStream]] =
    handle(request, parsedPath).map { result => result.copy(body = spec.outputBodyCodec.out(result.body)) }

  def parseAndHandle(
      request: HttpRequest,
  ): ZIO[R, implementation.DomainError, Option[HttpResponse[Send[OutputBody[ET]]]]] =
    spec.pathCodec.decodePath(request.path) match {
      case Some(parsedPath) => handle(request, parsedPath).asSome
      case None             => ZIO.none
    }

}
object Endpoint {

  type Projection[-R] = [ET <: EndpointType.Any] =>> Endpoint[R, ET]

  def make[R, T[_[_ <: EndpointType.Any]]](
      spec: T[EndpointSpec],
      impl: T[Implementation.Projection[R]],
  )(using
      zip: Zipper[T],
      mapK: MapK[T],
  ): T[Endpoint.Projection[R]] =
    mapK.mapK(Zipper.zip(spec, impl)) { [t <: EndpointType.Any] => (a: K11ET.Zip[EndpointSpec, Implementation.Projection[R]][t]) => Endpoint(a._1, a._2) }

}
