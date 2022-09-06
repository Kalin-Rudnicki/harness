package harness.sql

import zio.{Unzippable, Zippable}

trait ZipCodec[A, B] {
  type C
  def zip(a: A, b: B): C
  def unzip(c: C): (A, B)
}
object ZipCodec {

  type Aux[A, B, _C] = ZipCodec[A, B] { type C = _C }

  implicit def join[A, B, _C](implicit _zip: Zippable.Out[A, B, _C], _unzip: Unzippable.In[A, B, _C]): ZipCodec.Aux[A, B, _C] =
    new ZipCodec[A, B] {
      override type C = _C
      override def zip(a: A, b: B): _C = _zip.zip(a, b)
      override def unzip(c: _C): (A, B) = _unzip.unzip(c)
    }

}
