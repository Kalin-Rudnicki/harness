package harness.http.client

import cats.data.NonEmptyList
import cats.syntax.option.*
import harness.core.*
import harness.web.*
import harness.zio.*
import harness.zio.ZIOJsonInstances.catsNelJsonCodec
import zio.*
import zio.json.*

final class HttpResponse[ResponseBody] private (
    result: HttpResponse.Result[ResponseBody],
    // TODO (KR) : setCookies: Map[String, ???]
    bodyRef: Ref.Synchronized[Option[String]],
) extends ResponseOps[Any, Logger, ResponseBody] { self =>

  val responseCode: HttpCode = self.result.responseCode
  val headers: Map[String, List[String]] = self.result.headers
  val contentLength: Option[Long] = self.result.contentLength
  val _rawBody: ResponseBody = result.body

  // =====| Content Length |=====

  def getContentLength: HTask[Long] = self.result.getContentLength
  def contentLengthInt: HTask[Option[Int]] = self.result.contentLengthInt
  def getContentLengthInt: HTask[Int] = self.result.getContentLengthInt

  // =====| Body |=====

  def forwardBodyToPath(path: Path): HTask[Long] = self.result.forwardBodyToPath(path)

  override def bodyAsString: HRIO[Logger, String] =
    self.bodyRef.modifyZIO {
      case b @ Some(value) => Logger.log.debug("Already loaded body").as((value, b))
      case None            => self.result.bodyAsStringImpl(self.result.body).map(b => (b, b.some))
    }

  // =====| Headers |=====

  def getHeader[T](header: String)(implicit decoder: StringDecoder[T]): HTask[T] =
    self.headers.get(header.toLowerCase) match {
      case Some(value :: Nil) => ZIO.eitherNelToUserErrors(decoder.decodeAccumulating(value))
      case Some(Nil)          => ZIO.fail(HError.UserError(s"Unable to get value from header '$header', header exists but has no values"))
      case Some(values)       => ZIO.fail(HError.UserError(s"Unable to get value from header '$header', header exists but has multiple values (${values.size})", values.mkString("[", ", ", "]")))
      case None               => ZIO.fail(HError.UserError(s"Unable to get value from header '$header', header is missing"))
    }
  def findHeader[T](header: String)(implicit decoder: StringDecoder[T]): HTask[Option[T]] =
    self.headers.get(header.toLowerCase) match {
      case Some(value :: Nil) => ZIO.eitherNelToUserErrors(decoder.decodeAccumulating(value).map(_.some))
      case Some(Nil)          => ZIO.fail(HError.UserError(s"Unable to get value from header '$header', header exists but has no values")) // TODO (KR) : ZIO.none ?
      case Some(values)       => ZIO.fail(HError.UserError(s"Unable to get value from header '$header', header exists but has multiple values (${values.size})", values.mkString("[", ", ", "]")))
      case None               => ZIO.none
    }

  def getJsonHeader[T: JsonDecoder](header: String): HTask[T] = self.getHeader[T](header)(StringDecoder.fromJsonDecoder[T])
  def findJsonHeader[T: JsonDecoder](header: String): HTask[Option[T]] = self.findHeader[T](header)(StringDecoder.fromJsonDecoder[T])

  def getHeaderValues(header: String): HTask[List[String]] =
    self.headers.get(header.toLowerCase) match {
      case Some(values) => ZIO.succeed(values)
      case None         => ZIO.fail(HError.UserError(s"Response does not contain header '$header'"))
    }

  // =====| Other |=====

  override protected def getResponse: HRIO[Scope, HttpResponse[ResponseBody]] = ZIO.succeed(self)

  // =====| Show |=====

  def showHeaders: String =
    headers.map { (h, vs) => s"\n - $h:${vs.map { v => s"\n   - $v" }.mkString}" }.mkString("Headers:", "", "")

  // TODO (KR) : showSetCookies

  def show: HRIO[Logger, String] =
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

  def fromResult[ResponseBody](result: HttpResponse.Result[ResponseBody]): UIO[HttpResponse[ResponseBody]] =
    for {
      bodyRef <- Ref.Synchronized.make(Option.empty[String])
    } yield new HttpResponse(result, bodyRef)

  trait Result[ResponseBody] { self =>

    // =====|  |=====

    val responseCode: HttpCode
    protected val _headers: Map[String, List[String]]
    protected val _contentLength: Option[Long]
    val body: ResponseBody

    // =====|  |=====

    def bodyAsStringImpl(body: ResponseBody): HRIO[Logger, String]
    def forwardBodyToPath(path: Path): HTask[Long]

    // =====|  |=====

    final val headers: Map[String, List[String]] = self._headers.map { (k, vs) => (k.toLowerCase, vs) }
    final val contentLength: Option[Long] = self._contentLength.flatMap(cl => Option.when(cl != -1L)(cl))

    final def getContentLength: HTask[Long] =
      self.contentLength match {
        case Some(contentLength) => ZIO.succeed(contentLength)
        case None                => ZIO.fail(HError.InternalDefect("Expected to have content length, but it was not present in response"))
      }

    final def contentLengthInt: HTask[Option[Int]] =
      ZIO.foreach(self.contentLength)(safeConvertContentLength)

    final def getContentLengthInt: HTask[Int] =
      self.getContentLength.flatMap(safeConvertContentLength)

  }

  private def safeConvertContentLength(contentLength: Long): HTask[Int] =
    if (contentLength <= Int.MaxValue) ZIO.succeed(contentLength.toInt)
    else ZIO.fail(HError.InternalDefect(s"ContentLength ($contentLength) is too large to fit inside an Int"))

}