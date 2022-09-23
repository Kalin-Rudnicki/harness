package harness.web.client

import cats.data.NonEmptyList
import cats.syntax.option.*
import harness.core.*
import harness.web.*
import harness.zio.*
import org.scalajs.dom.XMLHttpRequest
import scala.scalajs.js
import scala.scalajs.js.URIUtils.encodeURIComponent
import zio.*
import zio.json.*

object HttpRequest {

  def apply(
      method: HttpMethod,
      baseUrl: String,
  ): Stage1 =
    Stage1(
      method,
      baseUrl,
      Nil,
      Nil,
    )

  inline def get(baseUrl: String): Stage1 = HttpRequest(HttpMethod.GET, baseUrl)
  inline def post(baseUrl: String): Stage1 = HttpRequest(HttpMethod.POST, baseUrl)

  final class Stage1 private[HttpRequest] (
      method: HttpMethod,
      baseUrl: String,
      paramList: List[(String, String)],
      headers: List[(String, String)],
  ) { self =>

    // =====| params |=====

    def params(params: (String, String)*): Stage1 =
      Stage1(
        method,
        baseUrl,
        params.toList ::: paramList,
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
        baseUrl,
        paramList,
        (k, encoder.encode(v)) :: headers,
      )

    def jsonHeader[V](k: String, v: V)(implicit encoder: JsonEncoder[V]): Stage1 =
      Stage1(
        method,
        baseUrl,
        paramList,
        (k, encoder.encodeJson(v, None).toString) :: headers,
      )

    // =====| body |=====

    def noBody: Stage2 =
      Stage2(
        method,
        baseUrl,
        paramList,
        headers,
        None,
      )

    def rawBody(body: js.Any): Stage2 =
      Stage2(
        method,
        baseUrl,
        paramList,
        headers,
        body.some,
      )

    def body[V](v: V)(implicit encoder: StringEncoder[V]): Stage2 =
      Stage2(
        method,
        baseUrl,
        paramList,
        headers,
        (encoder.encode(v): js.Any).some,
      )

    def jsonBody[V](v: V)(implicit encoder: JsonEncoder[V]): Stage2 =
      Stage2(
        method,
        baseUrl,
        paramList,
        headers,
        (encoder.encodeJson(v, None).toString: js.Any).some,
      )

  }

  final class Stage2 private[HttpRequest] (
      method: HttpMethod,
      baseUrl: String,
      params: List[(String, String)],
      headers: List[(String, String)],
      body: Option[js.Any],
  ) {

    private def build[Response](f: (HttpCode, String) => HTaskN[Response]): HTaskN[Response] = {
      def encodeParam(p: (String, String)): String =
        s"${encodeURIComponent(p._1)}=${encodeURIComponent(p._2)}"

      ZIO.asyncZIO[Any, NonEmptyList[HError], Response] { register =>
        ZIO.hAttemptNel("Error getting http response") {
          val xhr = new XMLHttpRequest
          xhr.open(
            method = method.method,
            url = s"$baseUrl${if (params.isEmpty) "" else s"?${params.reverseMap(encodeParam).mkString("&")}"}",
            async = true,
          )
          headers.foreach { xhr.setRequestHeader(_, _) }
          xhr.onload = { _ =>
            register(f(HttpCode(xhr.status), xhr.responseText))
          }

          body match {
            case Some(body) => xhr.send(body)
            case None       => xhr.send()
          }
        }
      }
    }

    def codeAndString: HTaskN[(HttpCode, String)] =
      build { (c, b) => ZIO.succeed((c, b)) }

    def string200: HTaskN[String] =
      build {
        case (HttpCode.`200`, b) => ZIO.succeed(b)
        case (_, b) =>
          JsonDecoder[List[String]].decodeJson(b) match {
            case Right(error0 :: errorN) => ZIO.fail(NonEmptyList(error0, errorN).map(HError.UserError(_, "Error result from Http Request")))
            case _                       => ZIO.failNel(HError.UserError(b, "Error result from Http Request"))
          }
      }

    def unit200: HTaskN[Unit] =
      build {
        case (HttpCode.`200`, _) => ZIO.unit
        case (_, b) =>
          JsonDecoder[List[String]].decodeJson(b) match {
            case Right(error0 :: errorN) => ZIO.fail(NonEmptyList(error0, errorN).map(HError.UserError(_, "Error result from Http Request")))
            case _                       => ZIO.failNel(HError.UserError(b, "Error result from Http Request"))
          }
      }

    def response[Response](implicit decoder: StringDecoder[Response]): HTaskN[Response] =
      string200.flatMap {
        decoder.decodeAccumulating(_) match {
          case Right(value) => ZIO.succeed(value)
          case Left(errors) => ZIO.fail(errors.map(HError.InternalDefect(_)))
        }
      }

    def jsonResponse[Response](implicit decoder: JsonDecoder[Response]): HTaskN[Response] =
      string200.flatMap {
        decoder.decodeJson(_) match {
          case Right(value) => ZIO.succeed(value)
          case Left(error)  => ZIO.failNel(HError.InternalDefect(error))
        }
      }

  }

}
