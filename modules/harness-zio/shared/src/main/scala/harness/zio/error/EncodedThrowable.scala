package harness.zio.error

import zio.json.*

final case class EncodedThrowable(
    `class`: String,
    message: Option[String],
    cause: Option[EncodedThrowable],
) {
  def toThrowable: Throwable = new Throwable(message.getOrElse(`class`), cause.map(_.toThrowable).orNull)
}
object EncodedThrowable {

  def fromThrowable(throwable: Throwable): EncodedThrowable =
    EncodedThrowable(
      throwable.getClass.getName,
      Option(throwable.getMessage),
      Option(throwable.getCause).map(EncodedThrowable.fromThrowable),
    )

  implicit val jsonCodec: JsonCodec[EncodedThrowable] = DeriveJsonCodec.gen

}
