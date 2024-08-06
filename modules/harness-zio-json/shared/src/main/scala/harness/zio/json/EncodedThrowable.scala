package harness.zio.json

import harness.core.*
import harness.zio.HTag
import zio.json.*

final case class EncodedThrowable(
    classTag: HTag[?],
    message: String,
    cause: Option[EncodedThrowable],
) extends Throwable {
  override def getMessage: String = this.toJsonPretty
  override def getCause: Throwable = cause.orNull
}
object EncodedThrowable {

  def fromThrowable(throwable: Throwable): EncodedThrowable =
    throwable match {
      case throwable: EncodedThrowable => throwable
      case _ =>
        EncodedThrowable(
          HTag.fromClass(throwable.getClass),
          throwable.safeGetMessage,
          Option(throwable.getCause).map(EncodedThrowable.fromThrowable),
        )
    }

  implicit val jsonCodec: JsonCodec[EncodedThrowable] = DeriveJsonCodec.gen

  implicit val throwableJsonCodec: JsonCodec[Throwable] =
    EncodedThrowable.jsonCodec.transform(identity[Throwable], EncodedThrowable.fromThrowable)

}
