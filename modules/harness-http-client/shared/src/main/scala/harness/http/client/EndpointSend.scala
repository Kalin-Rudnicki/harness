package harness.http.client

import cats.syntax.option.*
import harness.core.*
import harness.endpoint.spec.*
import harness.endpoint.typeclass.MapK
import harness.endpoint.types.*
import harness.endpoint.types.Types.*
import harness.zio.{Path as _, *}
import zio.*

final class EndpointSend[ET <: EndpointType.Any](url: String, spec: EndpointSpec[ET]) {

  // TODO (KR) : encode URI
  private def encodeCookie(k: String, v: String): String =
    s"$k=$v"

  private def sendWith(
      p: Path[ET],
      q: Query[ET],
      a: Option[Auth[ET]],
      h: Header[ET],
      b: Send[InputBody[ET]],
  ): ZIO[HttpClient & Logger & Telemetry & Scope, Error[ET], Receive[OutputBody[ET]]] = {
    val paths = spec.pathCodec.encodePath(p)
    val queries = spec.queryCodec.encode(q)
    val (hHeaders, hCookies) = spec.headerCodec.encode(h)
    val (aHeaders, aCookies) = a match {
      case Some(a) => spec.authHeaderCodec.encode(a)
      case None    => (Map.empty[String, List[String]], Map.empty[String, String])
    }

    val allCookies = hCookies ++ aCookies
    val allHeaders = aHeaders ++ hHeaders
    val allHeadersAndCookies =
      if (allCookies.isEmpty) allHeaders
      else allHeaders.updated("Cookie", allCookies.toList.map(encodeCookie(_, _)).mkString("; ") :: Nil)

    val reqParams = HttpRequestParams(
      method = spec.method,
      url = url,
      paths = paths,
      queryParams = queries.toList.flatMap { case (k, vs) => vs.map((k, _)) },
      headers = allHeadersAndCookies,
    )

    ZIO.serviceWithZIO[HttpClient](_.send[ET](spec.inputBodyCodec, spec.outputBodyCodec, spec.errorCodec)(reqParams, b))
  }

  /**
    * Scoped versions of functions are needed if you want to interact with a raw output stream
    */
  def sendWithAuthScoped(p: Path[ET], q: Query[ET], a: Auth[ET], h: Header[ET], b: Send[InputBody[ET]]): ZIO[HttpClient & Logger & Telemetry & Scope, Error[ET], Receive[OutputBody[ET]]] =
    sendWith(p, q, a.some, h, b)

  /**
    * Scoped versions of functions are needed if you want to interact with a raw output stream
    */
  def sendWithoutAuthScoped(p: Path[ET], q: Query[ET], h: Header[ET], b: Send[InputBody[ET]]): ZIO[HttpClient & Logger & Telemetry & Scope, Error[ET], Receive[OutputBody[ET]]] =
    sendWith(p, q, None, h, b)

  /**
    * Scoped versions of functions are needed if you want to interact with a raw output stream
    */
  def sendWithAuth(p: Path[ET], q: Query[ET], a: Auth[ET], h: Header[ET], b: Send[InputBody[ET]]): ZIO[HttpClient & Logger & Telemetry, Error[ET], Receive[OutputBody[ET]]] =
    ZIO.scoped { sendWith(p, q, a.some, h, b) }

  /**
    * Scoped versions of functions are needed if you want to interact with a raw output stream
    */
  def sendWithoutAuth(p: Path[ET], q: Query[ET], h: Header[ET], b: Send[InputBody[ET]]): ZIO[HttpClient & Logger & Telemetry, Error[ET], Receive[OutputBody[ET]]] =
    ZIO.scoped { sendWith(p, q, None, h, b) }

}
object EndpointSend {

  def make[T[_[_ <: EndpointType.Any]]](url: String, spec: T[EndpointSpec])(implicit mapK: MapK[T]): T[EndpointSend] =
    mapK.mapK(spec) { [t <: EndpointType.Any] => (s: EndpointSpec[t]) => new EndpointSend[t](url, s) }

}

implicit class EndpointSendWithAuthOps[ET <: EndpointType.Any, O](es: EndpointSend[ET])(implicit zip: Zip5.Out[Path[ET], Query[ET], Auth[ET], Header[ET], Send[InputBody[ET]], O]) {

  def apply(o: O): ZIO[HttpClient & Logger & Telemetry, Error[ET], Receive[OutputBody[ET]]] = {
    val (p, q, a, h, b) = zip.unzip(o)
    es.sendWithAuth(p, q, a, h, b)
  }

  /* format: off */

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

  def apply[A1, A2, A3, A4, A5, A6](a1: A1, a2: A2, a3: A3, a4: A4, a5: A5, a6: A6)(using ev: (A1, A2, A3, A4, A5, A6) <:< O): ZIO[HttpClient & Logger & Telemetry, Error[ET], Receive[OutputBody[ET]]] =
    apply(ev((a1, a2, a3, a4, a5, a6)))

  def apply[A1, A2, A3, A4, A5, A6, A7](a1: A1, a2: A2, a3: A3, a4: A4, a5: A5, a6: A6, a7: A7)(using ev: (A1, A2, A3, A4, A5, A6, A7) <:< O): ZIO[HttpClient & Logger & Telemetry, Error[ET], Receive[OutputBody[ET]]] =
    apply(ev((a1, a2, a3, a4, a5, a6, a7)))

  def apply[A1, A2, A3, A4, A5, A6, A7, A8](a1: A1, a2: A2, a3: A3, a4: A4, a5: A5, a6: A6, a7: A7, a8: A8)(using ev: (A1, A2, A3, A4, A5, A6, A7, A8) <:< O): ZIO[HttpClient & Logger & Telemetry, Error[ET], Receive[OutputBody[ET]]] =
    apply(ev((a1, a2, a3, a4, a5, a6, a7, a8)))

  def apply[A1, A2, A3, A4, A5, A6, A7, A8, A9](a1: A1, a2: A2, a3: A3, a4: A4, a5: A5, a6: A6, a7: A7, a8: A8, a9: A9)(using ev: (A1, A2, A3, A4, A5, A6, A7, A8, A9) <:< O): ZIO[HttpClient & Logger & Telemetry, Error[ET], Receive[OutputBody[ET]]] =
    apply(ev((a1, a2, a3, a4, a5, a6, a7, a8, a9)))

  def apply[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](a1: A1, a2: A2, a3: A3, a4: A4, a5: A5, a6: A6, a7: A7, a8: A8, a9: A9, a10: A10)(using ev: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10) <:< O): ZIO[HttpClient & Logger & Telemetry, Error[ET], Receive[OutputBody[ET]]] =
    apply(ev((a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)))

  /* format: on */

}

implicit class EndpointSendWithoutAuthOps[ET <: EndpointType.Any, O](es: EndpointSend[ET])(implicit zip: Zip4.Out[Path[ET], Query[ET], Header[ET], Send[InputBody[ET]], O]) {

  /* format: off */

  object withoutAuth {

    def apply(o: O): ZIO[HttpClient & Logger & Telemetry, Error[ET], Receive[OutputBody[ET]]] = {
      val (p, q, h, b) = zip.unzip(o)
      es.sendWithoutAuth(p, q, h, b)
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

    def apply[A1, A2, A3, A4, A5, A6](a1: A1, a2: A2, a3: A3, a4: A4, a5: A5, a6: A6)(using ev: (A1, A2, A3, A4, A5, A6) <:< O): ZIO[HttpClient & Logger & Telemetry, Error[ET], Receive[OutputBody[ET]]] =
      apply(ev((a1, a2, a3, a4, a5, a6)))

    def apply[A1, A2, A3, A4, A5, A6, A7](a1: A1, a2: A2, a3: A3, a4: A4, a5: A5, a6: A6, a7: A7)(using ev: (A1, A2, A3, A4, A5, A6, A7) <:< O): ZIO[HttpClient & Logger & Telemetry, Error[ET], Receive[OutputBody[ET]]] =
      apply(ev((a1, a2, a3, a4, a5, a6, a7)))

    def apply[A1, A2, A3, A4, A5, A6, A7, A8](a1: A1, a2: A2, a3: A3, a4: A4, a5: A5, a6: A6, a7: A7, a8: A8)(using ev: (A1, A2, A3, A4, A5, A6, A7, A8) <:< O): ZIO[HttpClient & Logger & Telemetry, Error[ET], Receive[OutputBody[ET]]] =
      apply(ev((a1, a2, a3, a4, a5, a6, a7, a8)))

    def apply[A1, A2, A3, A4, A5, A6, A7, A8, A9](a1: A1, a2: A2, a3: A3, a4: A4, a5: A5, a6: A6, a7: A7, a8: A8, a9: A9)(using ev: (A1, A2, A3, A4, A5, A6, A7, A8, A9) <:< O): ZIO[HttpClient & Logger & Telemetry, Error[ET], Receive[OutputBody[ET]]] =
      apply(ev((a1, a2, a3, a4, a5, a6, a7, a8, a9)))

    def apply[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](a1: A1, a2: A2, a3: A3, a4: A4, a5: A5, a6: A6, a7: A7, a8: A8, a9: A9, a10: A10)(using ev: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10) <:< O): ZIO[HttpClient & Logger & Telemetry, Error[ET], Receive[OutputBody[ET]]] =
      apply(ev((a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)))

  }

  /* format: on */

}
