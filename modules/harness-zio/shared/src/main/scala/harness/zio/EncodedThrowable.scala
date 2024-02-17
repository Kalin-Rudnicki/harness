package harness.zio

import zio.json.*

final case class EncodedThrowable(
    className: String,
    message: String,
    cause: Option[EncodedThrowable],
) extends Throwable
object EncodedThrowable {

  def fromThrowable(throwable: Throwable): EncodedThrowable =
    throwable match {
      case throwable: EncodedThrowable => throwable
      case _ =>
        EncodedThrowable(
          throwable.getClass.getName,
          Option(throwable.getMessage).getOrElse(throwable.toString),
          Option(throwable.getCause).map(EncodedThrowable.fromThrowable),
        )
    }

  implicit val jsonCodec: JsonCodec[EncodedThrowable] = DeriveJsonCodec.gen

  implicit val throwableJsonCodec: JsonCodec[Throwable] =
    EncodedThrowable.jsonCodec.transform(identity[Throwable], EncodedThrowable.fromThrowable)

}
