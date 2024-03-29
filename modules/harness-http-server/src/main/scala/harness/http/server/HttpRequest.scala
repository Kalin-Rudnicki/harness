package harness.http.server

import cats.syntax.either.*
import cats.syntax.traverse.*
import com.sun.net.httpserver.HttpExchange
import harness.core.*
import harness.http.server.error.DecodingFailure
import harness.web.*
import harness.zio.*
import java.io.{ByteArrayInputStream, InputStream}
import java.net.{InetSocketAddress, URLDecoder}
import java.util.UUID
import scala.jdk.CollectionConverters.*
import zio.*
import zio.json.{JsonDecoder, JsonEncoder}

final case class HttpRequest(
    requestId: UUID,
    method: HttpMethod,
    path: List[String],
    queries: Map[String, String],
    headers: Map[String, List[String]],
    cookies: Map[String, String],
    rawInputStream: InputStream,
    remoteAddress: InetSocketAddress,
) {
  val pathString: String = path.mkString("/", "/", "")
}
object HttpRequest {

  // =====| Public API |=====

  inline def service: URIO[HttpRequest, HttpRequest] = ZIO.service[HttpRequest]

  val path: URIO[HttpRequest, List[String]] = HttpRequest.service.map(_.path)
  val pathString: URIO[HttpRequest, String] = HttpRequest.service.map(_.pathString)

  val remoteAddress: URIO[HttpRequest, InetSocketAddress] = HttpRequest.service.map(_.remoteAddress)

  object query extends Lookup("query-param", req => name => req.queries.get(name).asRight) {

    def logAll(logLevel: Logger.LogLevel): URIO[HttpRequest & Logger, Unit] =
      HttpRequest.service.flatMap { req =>
        Logger.log(
          logLevel,
          req.queries.toList.map { (header, value) => s"\n[$header]: $value" }.mkString("--- HTTP Request Query Params ---", "", ""),
        )
      }

  }

  object header
      extends Lookup(
        "header",
        { req => name =>
          req.headers.get(name.toLowerCase).traverse {
            case v :: Nil => v.asRight
            case _        => s"Headers with more than 1 value not supported ($name)".asLeft
          }
        },
      ) {

    def logAll(logLevel: Logger.LogLevel): URIO[HttpRequest & Logger, Unit] =
      HttpRequest.service.flatMap { req =>
        Logger.log(
          logLevel,
          req.headers.toList
            .map {
              case (header, value :: Nil) => s"\n[$header]: $value"
              case (header, values)       => s"\n[$header]:${values.map(v => s"\n  - $v").mkString}"
            }
            .mkString("--- HTTP Request Headers ---", "", ""),
        )
      }

  }

  object cookie extends Lookup("cookie", req => name => req.cookies.get(name).asRight) {

    def logAll(logLevel: Logger.LogLevel): URIO[HttpRequest & Logger, Unit] =
      HttpRequest.service.flatMap { req =>
        Logger.log(
          logLevel,
          req.cookies.toList.map { (header, value) => s"\n[$header]: $value" }.mkString("--- HTTP Request Cookies ---", "", ""),
        )
      }

  }

  def body[T: StringDecoder]: ZIO[HttpRequest, DecodingFailure, T] =
    for {
      req <- HttpRequest.service
      contentLength <- HttpRequest.header.find[Long]("Content-length")
      body <-
        contentLength match {
          case Some(contentLength) if contentLength > Int.MaxValue =>
            ZIO.dieMessage("Request body is too long to fit in a String")
          case Some(_) =>
            ZIO.attempt(String(req.rawInputStream.readAllBytes())).orDie.flatMap {
              StringDecoder[T].decode(_) match {
                case Right(value) => ZIO.succeed(value)
                case Left(error)  => ZIO.fail(DecodingFailure(error))
              }
            }
          case None =>
            ZIO.fail(DecodingFailure("Request is missing body"))
        }
    } yield body

  def jsonBody[T: JsonDecoder]: ZIO[HttpRequest, DecodingFailure, T] =
    HttpRequest.body[T](using StringDecoder.fromJsonDecoder[T])

  val rawBody: URIO[HttpRequest, InputStream] =
    HttpRequest.service.map(_.rawInputStream)

  // =====| Helpers |=====

  private[server] def read(exchange: HttpExchange, requestId: UUID): HttpRequest = {
    val uri = exchange.getRequestURI
    val headerMap = exchange.getRequestHeaders.asScala.toMap.map { (k, v) => (k.toLowerCase, v.asScala.toList) }

    def getMap(
        raw: Option[String],
        firstSplit: String,
        mapPair: String => String,
        mapValue: String => String,
    ): Map[String, String] =
      raw match {
        case Some(raw) =>
          raw
            .split(firstSplit)
            .map { pair =>
              mapPair(pair).split("=", 2) match {
                case Array(k, v) => (k, mapValue(v))
                case _           => throw new RuntimeException(s"Invalid pair: $pair")
              }
            }
            .toMap
        case None => Map.empty
      }

    HttpRequest(
      requestId = requestId,
      method = HttpMethod(exchange.getRequestMethod),
      path = uri.getPath.split("/").toList.filter(_.nonEmpty),
      queries = getMap(Option(uri.getRawQuery), "&", identity, URLDecoder.decode(_, "UTF-8")),
      headers = headerMap,
      cookies = getMap(headerMap.get("cookie").flatMap(_.headOption), ";", _.trim, identity),
      rawInputStream = exchange.getRequestBody,
      remoteAddress = exchange.getRemoteAddress,
    )
  }

  sealed abstract class Lookup(g: String, lookup: HttpRequest => String => Either[String, Option[String]]) {

    inline def apply[T](name: String)(implicit decoder: StringDecoder[T]): ZIO[HttpRequest, DecodingFailure, T] =
      get[T](name)

    def get[T](name: String)(implicit decoder: StringDecoder[T]): ZIO[HttpRequest, DecodingFailure, T] =
      HttpRequest.service.flatMap {
        lookup(_)(name) match {
          case Right(Some(value)) =>
            decoder.decode(value) match {
              case Right(value) => ZIO.succeed(value)
              case Left(error)  => ZIO.fail(DecodingFailure(error))
            }
          case Right(None) => ZIO.fail(DecodingFailure(s"Missing required $g '$name'"))
          case Left(error) => ZIO.fail(DecodingFailure(error))
        }
      }

    def find[T](name: String)(implicit decoder: StringDecoder[T]): ZIO[HttpRequest, DecodingFailure, Option[T]] =
      HttpRequest.service.flatMap {
        lookup(_)(name) match {
          case Right(Some(value)) =>
            decoder.decode(value) match {
              case Right(value) => ZIO.some(value)
              case Left(error)  => ZIO.fail(DecodingFailure(error))
            }
          case Right(None) => ZIO.none
          case Left(error) => ZIO.fail(DecodingFailure(error))
        }
      }

  }

  // =====| Builder |=====

  object builder {

    def apply(
        method: HttpMethod,
    )(
        path: String*,
    ): Stage1 =
      Stage1(
        method,
        path.toList,
        Map.empty,
        Map.empty,
      )

    inline def get(path: String*): Stage1 = HttpRequest.builder(HttpMethod.GET)(path*)
    inline def post(path: String*): Stage1 = HttpRequest.builder(HttpMethod.POST)(path*)

    final class Stage1 private[builder] (
        method: HttpMethod,
        path: List[String],
        paramMap: Map[String, String],
        headers: Map[String, String],
    ) { self =>

      // =====| params |=====

      def params(params: (String, String)*): Stage1 =
        Stage1(
          method,
          path,
          paramMap ++ params.toMap,
          headers,
        )

      inline def param[V](k: String, v: V)(implicit encoder: StringEncoder[V]): Stage1 =
        self.params(k -> encoder.encode(v))

      inline def optParam[V](k: String, v: Option[V])(implicit encoder: StringEncoder[V]): Stage1 =
        v match {
          case Some(v) => self.param(k, v)
          case None    => self
        }

      // =====| headers |=====

      def header[V](k: String, v: V)(implicit encoder: StringEncoder[V]): Stage1 =
        Stage1(
          method,
          path,
          paramMap,
          headers + (k -> encoder.encode(v)),
        )

      def optHeader[V](k: String, v: Option[V])(implicit encoder: StringEncoder[V]): Stage1 =
        v match {
          case Some(v) => self.header(k, v)
          case None    => self
        }

      def jsonHeader[V](k: String, v: V)(implicit encoder: JsonEncoder[V]): Stage1 =
        Stage1(
          method,
          path,
          paramMap,
          headers + (k -> encoder.encodeJson(v, None).toString),
        )

      def optJsonHeader[V](k: String, v: Option[V])(implicit encoder: JsonEncoder[V]): Stage1 =
        v match {
          case Some(v) => self.jsonHeader(k, v)
          case None    => self
        }

      // =====| body |=====

      def noBody: HttpRequest =
        HttpRequest(
          requestId = UUID.randomUUID,
          method = method,
          path = path,
          queries = paramMap,
          headers = headers.map { (k, v) => (k, v :: Nil) },
          cookies = Map.empty,
          rawInputStream = InputStream.nullInputStream,
          remoteAddress = InetSocketAddress("localhost", 0),
        )

      def rawBody(body: String): HttpRequest =
        HttpRequest(
          requestId = UUID.randomUUID,
          method = method,
          path = path,
          queries = paramMap,
          headers = (headers + ("Content-length" -> body.length.toString)).map { (k, v) => (k, v :: Nil) },
          cookies = Map.empty,
          rawInputStream = new ByteArrayInputStream(body.getBytes),
          remoteAddress = InetSocketAddress("localhost", 0),
        )

      def body[V](v: V)(implicit encoder: StringEncoder[V]): HttpRequest =
        rawBody(encoder.encode(v))

      def jsonBody[V](v: V)(implicit encoder: JsonEncoder[V]): HttpRequest =
        rawBody(encoder.encodeJson(v, None).toString)

    }

  }

}
