package harness.core

import cats.data.NonEmptyList
import cats.syntax.either.*

final case class StringCodec[A](
    encoder: StringEncoder[A],
    decoder: StringDecoder[A],
) {

  def iemap[B](to: A => Either[String, B], from: B => A): StringCodec[B] =
    StringCodec(encoder.imap(from), decoder.flatMap(to(_).leftMap(NonEmptyList.one)))

}
object StringCodec {

  implicit def fromParts[A](implicit encoder: StringEncoder[A], decoder: StringDecoder[A]): StringCodec[A] =
    StringCodec[A](encoder, decoder)

}
