package harness.http.client

import harness.endpoint.error.DecodingFailure
import harness.endpoint.spec.*
import harness.endpoint.transfer.*
import harness.endpoint.types.*
import harness.endpoint.types.Types.*
import harness.web.*
import harness.zio.*
import java.net.{HttpURLConnection, URL, URLEncoder}
import scala.jdk.CollectionConverters.*
import zio.*

trait HttpClientPlatformSpecificImpl { self: HttpClientPlatformSpecific =>

  override val defaultClient: HttpClient =
    new HttpClient {

      private inline def encodeQueryParam(key: String, value: String): String =
        s"${URLEncoder.encode(key, "UTF-8")}=${URLEncoder.encode(value, "UTF-8")}"

      private inline def encodeQueryParams(queryParams: List[(String, String)]): String =
        if (queryParams.isEmpty) ""
        else queryParams.map(encodeQueryParam(_, _)).mkString("?", "&", "")

      private inline def makeUrl(url: String, paths: List[String], queryParams: List[(String, String)]): RIO[Logger, URL] =
        ZIO.attempt { new URL(s"$url${paths.map(URLEncoder.encode(_, "UTF-8")).mkString("/", "/", "")}${encodeQueryParams(queryParams)}") }

      private inline def getConnection(url: URL): RIO[Scope, HttpURLConnection] =
        ZIO.attempt { url.openConnection() }.mapError(new RuntimeException(s"Error opening URL connection for: $url", _)).flatMap {
          case httpURLConnection: HttpURLConnection => ZIO.acquireRelease(ZIO.succeed(httpURLConnection)) { con => ZIO.attempt { con.disconnect() }.orDie }
          case tmpCon                               => ZIO.fail(new RuntimeException(s"Expected HttpURLConnection, but got: ${tmpCon.getClass.getName}"))
        }

      private inline def setMethod(con: HttpURLConnection, method: HttpMethod): Task[Unit] =
        ZIO.attempt { con.setRequestMethod(method.method) }

      private inline def setHeaders(con: HttpURLConnection, headers: Map[String, List[String]]): Task[Unit] =
        ZIO.foreachDiscard(headers.toList) {
          case (header, value :: Nil) => ZIO.attempt { con.setRequestProperty(header, value) }
          case (header, values)       => ZIO.foreachDiscard(values) { value => ZIO.attempt { con.addRequestProperty(header, value) } }
        }

      private def setBody(con: HttpURLConnection, body: OutputStream): Task[Unit] =
        body match {
          case OutputStream.Empty => ZIO.unit
          case body: OutputStream.NonEmpty =>
            ZIO.scoped {
              for {
                os: java.io.OutputStream <- ZIO.fromAutoCloseable(ZIO.attempt(con.getOutputStream))
                _ <- body match {
                  case OutputStream.Str(string)        => ZIO.attempt { os.write(string.getBytes) }
                  case OutputStream.File(path)         => ZIO.scoped { path.inputStream.flatMap { is => ZIO.attempt { is.transferTo(os) } } }
                  case OutputStream.ForwardRaw(stream) => ZIO.attempt { stream.transferTo(os) }
                }
                _ <- ZIO.attempt { os.flush() }.mapError(new RuntimeException("Error flushing request body", _))
              } yield ()
            }
        }

      private inline def getResponseParamsAndStream(con: HttpURLConnection): RIO[Logger & Scope, (HttpResponseParams, InputStream)] =
        for {
          responseCode: HttpCode <- ZIO.attempt { con.getResponseCode() }.mapBoth(new RuntimeException("Error getting response code", _), HttpCode(_))
          contentLength: Long <- ZIO.attempt { con.getContentLengthLong() }.mapError(new RuntimeException("Error getting content length", _))
          javaHeaders: java.util.Map[String, java.util.List[String]] <- ZIO.attempt { con.getHeaderFields() }
          scalaHeaders: Map[String, List[String]] = javaHeaders.asScala.toList.flatMap { case (k, v) => Option(k).map(k => (k.toLowerCase, v.asScala.toList)) }.toMap
          body: java.io.InputStream <-
            if (responseCode.is2xx) ZIO.acquireClosable { ZIO.attempt { con.getInputStream() } }
            else if (responseCode.is4xxOr5xx) ZIO.acquireClosable { ZIO.attempt { con.getErrorStream() } }
            else
              Logger.log.warning("Response is not 2xx, 4xx, or 5xx. Assuming to use 'inputStream' and not 'errorStream'") *>
                ZIO.acquireClosable { ZIO.attempt { con.getInputStream() } }
        } yield (
          HttpResponseParams(responseCode, scalaHeaders, Option.when(contentLength >= 0L)(contentLength)),
          InputStream(body),
        )

      private def getBody[ET <: EndpointType.Any](
          outputBodySchema: BodySchema[OutputBody[ET]],
          errorSchema: ErrorSchema[Error[ET]],
      )(
          requestParams: HttpRequestParams,
          responseParams: HttpResponseParams,
          body: InputStream,
      ): IO[Error[ET], Receive[OutputBody[ET]]] =
        if (responseParams.responseCode.is2xx)
          outputBodySchema.in(responseParams.contentLength.toList, body.wrapped).orDie
        else
          body.readString.orDie.flatMap { stringBody =>
            errorSchema.decode(responseParams.responseCode, stringBody) match {
              case Some(Right(error)) => ZIO.fail(error)
              case Some(Left(error))  => ZIO.die(DecodingFailure(DecodingFailure.Source.Body, error))
              case None =>
                responseParams.responseCode match {
                  case HttpCode.`404` =>
                    ZIO.dieMessage(s"Target HTTP server does not handle: ${requestParams.paths.mkString("/", "/", "")}\n${stringBody}")
                  case HttpCode.`405` =>
                    ZIO.dieMessage(s"Target HTTP server does not handle ${requestParams.method.method}: ${requestParams.paths.mkString("/", "/", "")}\n${stringBody}")
                  case _ =>
                    ZIO.die(DecodingFailure(DecodingFailure.Source.Body, s"Unexpected response code: ${responseParams.responseCode}"))
                }
            }
          }

      override def send_internal[ET <: EndpointType.Any](
          inputBodySchema: BodySchema[InputBody[ET]],
          outputBodySchema: BodySchema[OutputBody[ET]],
          errorSchema: ErrorSchema[Error[ET]],
      )(
          requestParams: HttpRequestParams,
          body: Send[InputBody[ET]],
      ): ZIO[Logger & Telemetry, Error[ET], Receive[OutputBody[ET]]] =
        ZIO.scoped {
          for {
            url <- makeUrl(requestParams.url, requestParams.paths, requestParams.queryParams).orDie
            _ <- Logger.log.debug(s"Sending HTTP request to: $url")
            con: HttpURLConnection <- getConnection(url).orDie
            _ <- setMethod(con, requestParams.method).orDie
            _ <- ZIO.attempt { con.setDoOutput(true) }.orDie
            _ <- setHeaders(con, requestParams.headers).orDie
            _ <- setBody(con, inputBodySchema.out(body)).orDie
            (responseParams, stream) <- getResponseParamsAndStream(con).orDie
            result <- getBody[ET](outputBodySchema, errorSchema)(requestParams, responseParams, stream)
          } yield result
        }

    }

}
