package harness.endpoint.error

import harness.core.*
import harness.endpoint.spec.NonBodyInputCodec
import harness.endpoint.spec.NonBodyInputCodec.{FailureReason, OtherSchema}
import harness.schema.{given, *}
import zio.json.*

final case class DecodingFailure(
    source: DecodingFailure.Source,
    message: String,
) extends Throwable {
  override def getMessage: String = s"Decoding failure with $source : $message"
}
object DecodingFailure {

  @jsonDiscriminator("type")
  sealed trait Source(final val show: String) {
    override def toString: String = show
  }
  object Source {

    final case class Query(key: String) extends Source(s"query param '$key'")
    final case class Header(key: String) extends Source(s"header '$key'")
    final case class Cookie(key: String) extends Source(s"cookie '$key'")
    final case class HeaderOrCookie(key: String) extends Source(s"header/cookie '$key'")
    case object Body extends Source("body")

    implicit val schema: JsonSchema[Source] = JsonSchema.derive

  }

  def fromFailure(fail: NonBodyInputCodec.Result.Fail): DecodingFailure =
    DecodingFailure(
      fail.schema match {
        case _: OtherSchema.QuerySchema          => Source.Query(fail.schema.key)
        case _: OtherSchema.HeaderSchema         => Source.Header(fail.schema.key)
        case _: OtherSchema.CookieSchema         => Source.Cookie(fail.schema.key)
        case _: OtherSchema.HeaderOrCookieSchema => Source.HeaderOrCookie(fail.schema.key)
      },
      fail.reason match {
        case FailureReason.Missing               => "missing required"
        case FailureReason.DoesNotAcceptMultiple => "does not accept multiple values"
        case FailureReason.DecodeFail(error)     => error
      },
    )

  implicit val schema: JsonSchema[DecodingFailure] = JsonSchema.derive

}
