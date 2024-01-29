package harness.sql.typeclass

import cats.data.EitherNel
import cats.syntax.either.*
import harness.core.Zip
import harness.sql.*

final case class RowCodec[T](encoder: RowEncoder[T], decoder: RowDecoder[T]) { self =>

  final def imap[T2](mf: T => T2)(cmf: T2 => T): RowCodec[T2] = RowCodec(self.encoder.cmap(cmf), self.decoder.map(mf))
  final def iemap[T2](mf: T => EitherNel[String, T2])(cmf: T2 => T): RowCodec[T2] = RowCodec(self.encoder.cmap(cmf), self.decoder.emap(mf))

  final def ~[T2](other: RowCodec[T2])(implicit z: Zip[T, T2]): RowCodec[z.Out] =
    RowCodec(self.encoder ~ other.encoder, self.decoder ~ other.decoder)

  final def optional: RowCodec[Option[T]] = RowCodec(encoder.optional, decoder.optional)

}
