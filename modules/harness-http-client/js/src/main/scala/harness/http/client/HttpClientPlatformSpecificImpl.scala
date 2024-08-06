package harness.http.client

import harness.endpoint.error.DecodingFailure
import harness.endpoint.spec.*
import harness.endpoint.transfer.*
import harness.endpoint.types.*
import harness.endpoint.types.Types.*
import harness.web.*
import harness.zio.*
import org.scalajs.dom.XMLHttpRequest
import scala.scalajs.js.URIUtils.encodeURIComponent
import zio.*

trait HttpClientPlatformSpecificImpl { self: HttpClientPlatformSpecific =>

  override val defaultClient: HttpClient =
    new HttpClient {

      private inline def encodeQueryParam(key: String, value: String): String =
        s"${encodeURIComponent(key)}=${encodeURIComponent(value)}"

      private inline def encodeQueryParams(queryParams: List[(String, String)]): String =
        if (queryParams.isEmpty) ""
        else queryParams.map(encodeQueryParam(_, _)).mkString("?", "&", "")

      private inline def makeUrl(url: String, paths: List[String], queryParams: List[(String, String)]): String =
        s"$url${paths.map(p => s"/${encodeURIComponent(p)}").mkString}${encodeQueryParams(queryParams)}"

      private inline def makeXHR: Task[XMLHttpRequest] =
        ZIO.attempt { new XMLHttpRequest }

      private inline def openXHR(xhr: XMLHttpRequest, method: HttpMethod, url: String, paths: List[String], queryParams: List[(String, String)]): Task[Unit] =
        ZIO.attempt {
          xhr.open(
            method = method.method,
            url = makeUrl(url, paths, queryParams),
            async = true,
          )
        }

      private inline def setHeaders(xhr: XMLHttpRequest, headers: Map[String, List[String]]): Task[Unit] =
        ZIO.foreachDiscard(headers.toList) { (k, vs) => ZIO.foreachDiscard(vs) { v => ZIO.attempt { xhr.setRequestHeader(k, v) } } }

      private inline def readResponseBody[ET <: EndpointType.Any](
          outputBodySchema: BodyCodec[OutputBody[ET]],
          errorSchema: ErrorSchema[Error[ET]],
      )(
          requestParams: HttpRequestParams,
          xhr: XMLHttpRequest,
      ): IO[Error[ET], Receive[OutputBody[ET]]] =
        for {
          responseCode: HttpCode <- ZIO.attempt { xhr.status }.mapBoth(new RuntimeException("Error getting response code", _), HttpCode(_)).orDie
          response <-
            if (responseCode.is2xx)
              outputBodySchema match {
                case BodyCodec.Encoded(schema) =>
                  ZIO.attempt { xhr.responseText }.orDie.flatMap {
                    schema.decode(_) match {
                      case Right(value) => ZIO.succeed(value)
                      case Left(error)  => ZIO.die(DecodingFailure(SchemaSource.Body :: Nil, DecodingFailure.Cause.DecodeFail(error)))
                    }
                  }
                case BodyCodec.None   => ZIO.unit
                case BodyCodec.Stream => ZIO.dieMessage("Can not handle Stream response")
              }
            else
              ZIO.attempt { xhr.responseText }.orDie.flatMap { stringBody =>
                errorSchema.decode(responseCode, stringBody) match {
                  case Some(Right(error)) => ZIO.fail(error)
                  case Some(Left(error))  => ZIO.die(DecodingFailure(SchemaSource.Body :: Nil, DecodingFailure.Cause.DecodeFail(error)))
                  case None =>
                    responseCode match {
                      case HttpCode.`404` =>
                        ZIO.dieMessage(s"Target HTTP server does not handle: ${requestParams.paths.mkString("/", "/", "")}\n$stringBody")
                      case HttpCode.`405` =>
                        ZIO.dieMessage(s"Target HTTP server does not handle ${requestParams.method.method}: ${requestParams.paths.mkString("/", "/", "")}\n$stringBody")
                      case _ =>
                        ZIO.die(DecodingFailure(SchemaSource.Body :: Nil, DecodingFailure.Cause.DecodeFail(s"Unexpected response code: $responseCode")))
                    }
                }
              }
        } yield response.asInstanceOf[Receive[OutputBody[ET]]]

      private inline def send(xhr: XMLHttpRequest, body: OutputStream): Task[Unit] = body match
        case OutputStream.Empty                     => ZIO.attempt { xhr.send() }
        case OutputStream.Str(string)               => ZIO.attempt { xhr.send(string) }
        case OutputStream.File(WrappedJsPath(file)) => ZIO.attempt { xhr.send(file) }
        case _                                      => ZIO.dieMessage("TODO : send of invalid body not supported")

      override def send_internal[ET <: EndpointType.Any](
          inputBodySchema: BodyCodec[InputBody[ET]],
          outputBodySchema: BodyCodec[OutputBody[ET]],
          errorSchema: ErrorSchema[Error[ET]],
      )(
          request: HttpRequestParams,
          body: Send[InputBody[ET]],
      ): IO[Error[ET], Receive[OutputBody[ET]]] =
        ZIO
          .asyncZIO[Any, Throwable, XMLHttpRequest] { register =>
            for {
              xhr <- makeXHR
              _ <- openXHR(xhr, request.method, request.url, request.paths, request.queryParams)
              _ <- setHeaders(xhr, request.headers)
              _ <- ZIO.attempt { xhr.onload = { _ => register(ZIO.succeed(xhr)) } }
              _ <- send(xhr, inputBodySchema.out(body))
            } yield ()
          }
          .orDie
          .flatMap { xhr =>
            readResponseBody[ET](outputBodySchema, errorSchema)(request, xhr)
          }

    }

}
