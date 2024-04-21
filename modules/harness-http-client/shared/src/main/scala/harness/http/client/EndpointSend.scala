package harness.http.client

import harness.core.*
import harness.endpoint.spec.*
import harness.endpoint.typeclass.MapK
import harness.endpoint.types.*
import harness.endpoint.types.Types.*
import harness.zio.*
import zio.*

final class EndpointSend[ET <: EndpointType.Any](url: String, spec: EndpointSpec[ET]) {

  private def sendWith(b: Send[InputBody[ET]])(
      f: => (List[String], Map[String, List[String]], Map[String, List[String]], Map[String, String]),
  ): ZIO[HttpClient & Logger & Telemetry & Scope, Error[ET], Receive[OutputBody[ET]]] =
    for {
      (paths, queries, headers, _) <- ZIO.succeed { f }
      rawQueries = queries.toList.sortBy(_._1).flatMap { case (k, vs) => vs.map((k, _)) }
      headersWithCookies = headers // TODO (KR) : add cookies
      reqParams = HttpRequestParams(spec.method, url, paths, rawQueries, headersWithCookies)
      result <- ZIO.serviceWithZIO[HttpClient](_.send[ET](spec.inputBodyCodec, spec.outputBodyCodec, spec.errorCodec)(reqParams, b))
    } yield result

  /**
    * Scoped versions of functions are needed if you want to interact with a raw output stream
    */
  def sendWithCookiesScoped(i: InputWithCookies[ET], b: Send[InputBody[ET]]): ZIO[HttpClient & Logger & Telemetry & Scope, Error[ET], Receive[OutputBody[ET]]] =
    sendWith(b) { spec.inputWithCookiesCodec.encode(i) }

  /**
    * Scoped versions of functions are needed if you want to interact with a raw output stream
    */
  def sendWithoutCookiesScoped(i: InputWithoutCookies[ET], b: Send[InputBody[ET]]): ZIO[HttpClient & Logger & Telemetry & Scope, Error[ET], Receive[OutputBody[ET]]] =
    sendWith(b) { spec.inputWithoutCookiesCodec.encode(i) }

  /**
    * Scoped versions of functions are needed if you want to interact with a raw output stream
    */
  def sendWithCookies(i: InputWithCookies[ET], b: Send[InputBody[ET]]): ZIO[HttpClient & Logger & Telemetry, Error[ET], Receive[OutputBody[ET]]] =
    ZIO.scoped { sendWith(b) { spec.inputWithCookiesCodec.encode(i) } }

  /**
    * Scoped versions of functions are needed if you want to interact with a raw output stream
    */
  def sendWithoutCookies(i: InputWithoutCookies[ET], b: Send[InputBody[ET]]): ZIO[HttpClient & Logger & Telemetry, Error[ET], Receive[OutputBody[ET]]] =
    ZIO.scoped { sendWith(b) { spec.inputWithoutCookiesCodec.encode(i) } }

}
object EndpointSend {

  def make[T[_[_ <: EndpointType.Any]]](url: String, spec: T[EndpointSpec])(implicit mapK: MapK[T]): T[EndpointSend] =
    mapK.mapK(spec) { [t <: EndpointType.Any] => (s: EndpointSpec[t]) => new EndpointSend[t](url, s) }

}

implicit class EndpointSendOps[ET <: EndpointType.Any, O](es: EndpointSend[ET])(implicit zip: Zip.Out[InputWithoutCookies[ET], Send[InputBody[ET]], O]) {

  def apply(o: O): ZIO[HttpClient & Logger & Telemetry, Error[ET], Receive[OutputBody[ET]]] = {
    val (a, b) = zip.unzip(o)
    es.sendWithoutCookies(a, b)
  }

  def apply()(using ev: Unit <:< O): ZIO[HttpClient & Logger & Telemetry, Error[ET], Receive[OutputBody[ET]]] =
    apply(ev(()))

  def apply[A1, A2](a1: A1, a2: A2)(using ev: (A1, A2) <:< O): ZIO[HttpClient & Logger & Telemetry, Error[ET], Receive[OutputBody[ET]]] =
    apply(ev((a1, a2)))

  def apply[A1, A2, A3](a1: A1, a2: A2, a3: A3)(using ev: (A1, A2, A3) <:< O): ZIO[HttpClient & Logger & Telemetry, Error[ET], Receive[OutputBody[ET]]] =
    apply(ev((a1, a2, a3)))

  def apply[A1, A2, A3, A4](a1: A1, a2: A2, a3: A3, a4: A4)(using ev: (A1, A2, A3, A4) <:< O): ZIO[HttpClient & Logger & Telemetry, Error[ET], Receive[OutputBody[ET]]] =
    apply(ev((a1, a2, a3, a4)))

  def apply[A1, A2, A3, A4, A5](a1: A1, a2: A2, a3: A3, a4: A4, a5: A5)(using ev: (A1, A2, A3, A4, A5) <:< O): ZIO[HttpClient & Logger & Telemetry, Error[ET], Receive[OutputBody[ET]]] =
    apply(ev((a1, a2, a3, a4, a5)))

}
