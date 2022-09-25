package harness.web.server

import cats.data.NonEmptyList
import cats.syntax.either.*
import cats.syntax.traverse.*
import com.sun.net.httpserver.HttpExchange
import harness.core.*
import harness.web.*
import harness.zio.*
import java.io.InputStream
import java.net.URI
import scala.jdk.CollectionConverters.*
import zio.*
import zio.json.JsonDecoder

final case class HttpRequest(
    method: HttpMethod,
    path: List[String],
    queries: Map[String, String],
    headers: Map[String, List[String]],
    cookies: Map[String, String],
    rawInputStream: InputStream,
)
object HttpRequest {

  // =====| Public API |=====

  object query extends Lookup("query-param", req => name => req.queries.get(name).asRight)
  object header
      extends Lookup(
        "header",
        { req => name =>
          req.headers.get(name).traverse {
            case v :: Nil => v.asRight
            case _        => HError.UserError(s"Malformed header '$name'").asLeft
          }
        },
      )
  object cookie extends Lookup("cookie", req => name => req.cookies.get(name).asRight)

  def body[T: StringDecoder]: HRION[HttpRequest, T] =
    for {
      req <- ZIO.service[HttpRequest]
      contentLength <- HttpRequest.header.find[Long]("Content-length")
      body <-
        contentLength match {
          case Some(contentLength) if contentLength > Int.MaxValue =>
            ZIO.failNel(HError.InternalDefect("Request body is too long to fit in a String"))
          case Some(_) =>
            ZIO.hAttemptNel("Error reading request body")(String(req.rawInputStream.readAllBytes())).flatMap {
              StringDecoder[T].decodeAccumulating(_) match {
                case Right(value) => ZIO.succeed(value)
                case Left(errors) => ZIO.fail(errors.map(HError.UserError(_)))
              }
            }
          case None =>
            ZIO.failNel(HError.UserError("Request is missing body"))
        }
    } yield body

  def jsonBody[T: JsonDecoder]: HRION[HttpRequest, T] =
    HttpRequest.body[T](using { str => JsonDecoder[T].decodeJson(str).leftMap(NonEmptyList.one) })

  def rawBody: HRION[HttpRequest, InputStream] =
    ZIO.service[HttpRequest].map(_.rawInputStream)

  // =====| Helpers |=====

  private[server] def read(exchange: HttpExchange): HttpRequest = {
    val uri = exchange.getRequestURI
    val headerMap = exchange.getRequestHeaders.asScala.toMap.map { (k, v) => (k, v.asScala.toList) }

    def getMap(raw: Option[String], firstSplit: String, map: String => String): Map[String, String] =
      raw match {
        case Some(raw) =>
          raw
            .split(firstSplit)
            .map { pair =>
              map(pair).split("=", 2) match {
                case Array(k, v) => (k, v)
                case _           => throw new RuntimeException(s"Invalid pair: $pair")
              }
            }
            .toMap
        case None => Map.empty
      }

    HttpRequest(
      method = HttpMethod(exchange.getRequestMethod),
      path = uri.getPath.split("/").toList.filter(_.nonEmpty),
      queries = getMap(Option(uri.getQuery), "&", identity),
      headers = headerMap,
      cookies = getMap(headerMap.get("Cookie").flatMap(_.headOption), ";", _.trim),
      rawInputStream = exchange.getRequestBody,
    )
  }

  sealed abstract class Lookup(g: String, lookup: HttpRequest => String => EitherError[Option[String]]) {

    inline def apply[T](name: String)(implicit decoder: StringDecoder[T]): HRION[HttpRequest, T] =
      get[T](name)

    def get[T](name: String)(implicit decoder: StringDecoder[T]): HRION[HttpRequest, T] =
      ZIO.service[HttpRequest].flatMap {
        lookup(_)(name) match {
          case Right(Some(value)) =>
            decoder.decodeAccumulating(value) match {
              case Right(value) => ZIO.succeed(value)
              case Left(errors) => ZIO.fail(errors.map(HError.UserError(_)))
            }
          case Right(None) => ZIO.failNel(HError.UserError(s"Missing required $g '$name'"))
          case Left(error) => ZIO.failNel(error)
        }
      }

    def find[T](name: String)(implicit decoder: StringDecoder[T]): HRION[HttpRequest, Option[T]] =
      ZIO.service[HttpRequest].flatMap {
        lookup(_)(name) match {
          case Right(Some(value)) =>
            decoder.decodeAccumulating(value) match {
              case Right(value) => ZIO.some(value)
              case Left(errors) => ZIO.fail(errors.map(HError.UserError(_)))
            }
          case Right(None) => ZIO.none
          case Left(error) => ZIO.failNel(error)
        }
      }

  }

}
