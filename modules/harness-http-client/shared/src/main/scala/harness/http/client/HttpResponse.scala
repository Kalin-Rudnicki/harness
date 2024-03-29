package harness.http.client

import cats.syntax.option.*
import harness.core.*
import harness.http.client.error.HeaderError
import harness.web.*
import harness.zio.*
import zio.*
import zio.json.*

sealed abstract class HttpResponse[ResponseBody] private (
    final val result: HttpResponse.Result[ResponseBody],
) extends ResponseOps.Builder1[Any, ResponseBody] { self =>

  final val responseCode: HttpCode = self.result.fields.responseCode
  final val headers: Map[String, List[String]] = self.result.fields.headers
  final val contentLength: Option[Long] = self.result.fields.contentLength
  final val body: ResponseBody = self.result.fields.body

  // =====| Content Length |=====

  final def getContentLength: Task[Long] = self.result.fields.getContentLength
  final def contentLengthInt: Task[Option[Int]] = self.result.fields.contentLengthInt
  final def getContentLengthInt: Task[Int] = self.result.fields.getContentLengthInt

  // =====| Body |=====

  def bodyAsString: RIO[Logger, String]

  final def withBody[ResponseBody2](body: ResponseBody2, bodyOps: HttpResponse.BodyOps[ResponseBody2]): HttpResponse[ResponseBody2] =
    HttpResponse.Const[ResponseBody2](
      HttpResponse.Result[ResponseBody2](
        self.result.fields.withBody(body),
        bodyOps,
      ),
    )

  final def toResponseStringBody: RIO[Logger, HttpResponse[String]] =
    bodyAsString.map(self.withBody(_, HttpResponse.BodyOps.forStringBody))

  // =====| Headers |=====

  def getHeader[T](header: String)(implicit decoder: StringDecoder[T]): Task[T] =
    findHeader[T](header).someOrFail(HeaderError(header, "Missing header"))
  def findHeader[T](header: String)(implicit decoder: StringDecoder[T]): Task[Option[T]] =
    self.headers.get(header.toLowerCase) match {
      case Some(value :: Nil) => ZIO.fromEither(decoder.decode(value)).mapBoth(e => HeaderError(header, s"Error decoding: $e"), _.some)
      case Some(Nil)          => ZIO.fail(HeaderError(header, "Header is present, but has no values")) // TODO (KR) : ZIO.none ?
      case Some(values)       => ZIO.fail(HeaderError(header, s"Header is present, but has multiple values (${values.size})"))
      case None               => ZIO.none
    }

  def getJsonHeader[T: JsonDecoder](header: String): Task[T] = self.getHeader[T](header)(StringDecoder.fromJsonDecoder[T])
  def findJsonHeader[T: JsonDecoder](header: String): Task[Option[T]] = self.findHeader[T](header)(StringDecoder.fromJsonDecoder[T])

  def getHeaderValues(header: String): Task[List[String]] =
    self.headers.get(header.toLowerCase) match {
      case Some(values) => ZIO.succeed(values)
      case None         => ZIO.fail(HeaderError(header, "Missing header"))
    }

  // =====| Other |=====

  override protected def getResponse: RIO[Scope, HttpResponse[ResponseBody]] = ZIO.succeed(self)

  // =====| Show |=====

  def showHeaders: String =
    headers.map { (h, vs) => s"\n - $h:${vs.map { v => s"\n   - $v" }.mkString}" }.mkString("Headers:", "", "")

  // TODO (KR) : showSetCookies

  def show: RIO[Logger, String] =
    self.bodyAsString.map { bodyString =>
      s"""Response Code: ${self.responseCode}
         |${self.showHeaders}
         |Content Length: ${contentLength.fold("not provided")(_.toStringCommas)}
         |Body:\n$bodyString""".stripMargin
    }

  override def toString: String =
    s"""Response Code: ${self.responseCode}
       |${self.showHeaders}
       |Content Length: ${contentLength.fold("not provided")(_.toStringCommas)}""".stripMargin

}
object HttpResponse {

  final class Cached[ResponseBody] private[HttpResponse] (
      result: Result[ResponseBody],
      bodyRef: Ref.Synchronized[Option[String]],
  ) extends HttpResponse[ResponseBody](result) { self =>

    override def bodyAsString: RIO[Logger, String] =
      self.bodyRef.modifyZIO {
        case b @ Some(value) => Logger.log.debug("Already loaded body").as((value, b))
        case None            => self.result.ops.getStringBody(self.result.fields).map(b => (b, b.some))
      }

  }

  final class Const[ResponseBody] private[HttpResponse] (
      result: Result[ResponseBody],
  ) extends HttpResponse[ResponseBody](result) { self =>
    override def bodyAsString: RIO[Logger, String] = self.result.ops.getStringBody(self.result.fields)
  }

  def fromResult[ResponseBody](result: HttpResponse.Result[ResponseBody]): UIO[HttpResponse[ResponseBody]] =
    for {
      bodyRef <- Ref.Synchronized.make(Option.empty[String])
    } yield new HttpResponse.Cached(result, bodyRef)

  final case class ResultFields[ResponseBody] private (
      responseCode: HttpCode,
      headers: Map[String, List[String]],
      contentLength: Option[Long],
      body: ResponseBody,
  ) { self =>

    def withBody[ResponseBody2](body: ResponseBody2): ResultFields[ResponseBody2] = self.copy(body = body)

    def getContentLength: Task[Long] =
      ZIO.getOrFailWith(new RuntimeException("Expected to have content length, but it was not present in response"))(contentLength)

    def contentLengthInt: Task[Option[Int]] =
      ZIO.foreach(self.contentLength)(safeConvertContentLength)

    def getContentLengthInt: Task[Int] =
      self.getContentLength.flatMap(safeConvertContentLength)

  }
  object ResultFields {

    def make[ResponseBody](
        responseCode: HttpCode,
        headers: Map[String, List[String]],
        contentLength: Option[Long],
        body: ResponseBody,
    ): ResultFields[ResponseBody] =
      ResultFields(
        responseCode,
        headers.map { (k, vs) => (k.toLowerCase, vs) },
        contentLength.flatMap(cl => Option.when(cl != -1L)(cl)),
        body,
      )

  }

  final case class BodyOps[ResponseBody](
      getStringBody: ResultFields[ResponseBody] => RIO[Logger, String],
      forwardBodyToPath: (Path, ResponseBody) => Task[Long],
  )
  object BodyOps {
    val forStringBody: BodyOps[String] =
      BodyOps[String](
        fields => ZIO.succeed(fields.body),
        (path, body) => path.writeString(body).as(body.length.toLong),
      )
  }

  final case class Result[ResponseBody](
      fields: ResultFields[ResponseBody],
      ops: BodyOps[ResponseBody],
  )

  private def safeConvertContentLength(contentLength: Long): Task[Int] =
    if (contentLength <= Int.MaxValue) ZIO.succeed(contentLength.toInt)
    else ZIO.fail(new RuntimeException(s"ContentLength ($contentLength) is too large to fit inside an Int"))

}
