package harness.sql.typeclass

import cats.data.EitherNel
import harness.core.Zip

final case class QueryCodecMany[T](encoder: QueryEncoderMany[T], decoder: QueryDecoderMany[T]) { self =>

  def imap[T2](mf: T => T2)(cmf: T2 => T): QueryCodecMany[T2] = QueryCodecMany(self.encoder.cmap(cmf), self.decoder.map(mf))
  def iemap[T2](mf: T => EitherNel[String, T2])(cmf: T2 => T): QueryCodecMany[T2] = QueryCodecMany(self.encoder.cmap(cmf), self.decoder.emap(mf))

  def ~[T2](other: QueryCodecMany[T2])(implicit z: Zip[T, T2]): QueryCodecMany[z.Out] =
    QueryCodecMany(self.encoder ~ other.encoder, self.decoder ~ other.decoder)

  def optional: QueryCodecMany[Option[T]] = QueryCodecMany(encoder.optional, decoder.optional)

}
object QueryCodecMany {

  implicit def fromSingle[T](implicit codec: QueryCodecSingle[T]): QueryCodecMany[T] = QueryCodecMany(QueryEncoderMany.fromSingle(codec.encoder), QueryDecoderMany.fromSingle(codec.decoder))

}
