package harness.zio.json

import harness.core.*
import zio.json.*

final case class EncodedThrowable(
    className: String,
    message: String,
    cause: Option[EncodedThrowable],
) extends Throwable {
  override def getMessage: String = this.toJsonPretty
}
object EncodedThrowable {

  def fromThrowable(throwable: Throwable): EncodedThrowable =
    throwable match {
      case throwable: EncodedThrowable => throwable
      case _ =>
        EncodedThrowable(
          throwable.getClass.getName,
          throwable.safeGetMessage,
          Option(throwable.getCause).map(EncodedThrowable.fromThrowable),
        )
    }

  implicit val jsonCodec: JsonCodec[EncodedThrowable] = DeriveJsonCodec.gen

  implicit val throwableJsonCodec: JsonCodec[Throwable] =
    EncodedThrowable.jsonCodec.transform(identity[Throwable], EncodedThrowable.fromThrowable)

}
