package harness.http.server

import com.sun.net.httpserver.HttpExchange
import harness.web.HttpMethod
import java.io.InputStream
import java.net.{InetSocketAddress, URLDecoder}
import java.util.UUID
import scala.jdk.CollectionConverters.*

final case class HttpRequest(
    requestId: UUID,
    method: HttpMethod,
    path: List[String],
    queries: Map[String, List[String]],
    headers: Map[String, List[String]],
    cookies: Map[String, String],
    rawInputStream: InputStream,
    remoteAddress: InetSocketAddress,
    contentLengths: List[Long],
) {
  val pathString: String = path.mkString("/", "/", "")
}
object HttpRequest {

  private[server] def read(exchange: HttpExchange, requestId: UUID): HttpRequest = {
    val uri = exchange.getRequestURI
    val headerMap: Map[String, List[String]] = exchange.getRequestHeaders.asScala.toMap.map { (k, v) => (k.toUpperCase, v.asScala.toList) }

    def getPairs(
        raw: Option[String],
        firstSplit: String,
        mapPair: String => String,
        mapValue: String => String,
    ): List[(String, String)] =
      raw match {
        case Some(raw) =>
          raw.split(firstSplit).toList.map { pair =>
            mapPair(pair).split("=", 2) match {
              case Array(k, v) => (k, mapValue(v))
              case _           => throw new RuntimeException(s"Invalid pair: $pair")
            }
          }
        case None => Nil
      }

    HttpRequest(
      requestId = requestId,
      method = HttpMethod(exchange.getRequestMethod),
      path = uri.getPath.split("/").toList.filter(_.nonEmpty),
      queries = getPairs(Option(uri.getRawQuery), "&", identity, URLDecoder.decode(_, "UTF-8")).groupMap(_._1)(_._2),
      headers = headerMap,
      cookies = getPairs(headerMap.get("cookie").flatMap(_.headOption), ";", _.trim, identity).map { case (k, v) => (k.toUpperCase, v) }.toMap,
      rawInputStream = exchange.getRequestBody,
      remoteAddress = exchange.getRemoteAddress,
      headerMap.getOrElse("content-length", Nil).map(l => l.toLongOption.getOrElse(throw new RuntimeException(s"Malformed content-length header:$l"))),
    )
  }

}
