package harness.sql

import cats.data.EitherNel
import cats.syntax.either.*
import shapeless3.deriving.*

final case class RowCodec[T](encoder: RowEncoder[T], decoder: RowDecoder[T]) {

  def encodeRow(t: T): List[ColT] = encoder.encodeRow(t)
  def decodeRow(cs: List[ColT]): EitherNel[String, (T, List[ColT])] = decoder.decodeRow(cs)

  final def imap[T2](mf: T => T2)(cmf: T2 => T): RowCodec[T2] = RowCodec(encoder.cmap(cmf), decoder.map(mf))
  final def iemap[T2](mf: T => EitherNel[String, T2])(cmf: T2 => T): RowCodec[T2] = RowCodec(encoder.cmap(cmf), decoder.emap(mf))

}
object RowCodec {

  def fromColCodec[T](cc: ColCodec[T]): RowCodec[T] = RowCodec(RowEncoder.fromColEncoder(cc.encoder), RowDecoder.fromColDecoder(cc.decoder))

}
