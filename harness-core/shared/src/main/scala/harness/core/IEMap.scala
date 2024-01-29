package harness.core

import cats.data.EitherNel

trait IEMap[A, B] {
  def toOrFail(a: A): EitherNel[String, B]
  def from(b: B): A
}
object IEMap {

  def make[A]: Builder1[A] = new Builder1

  final class Builder1[A] private[IEMap] {
    def apply[B](toOrFail: A => EitherNel[String, B]): Builder2[A, B] = Builder2(toOrFail)
  }

  final class Builder2[A, B] private[IEMap] (_toOrFail: A => EitherNel[String, B]) {
    def apply(_from: B => A): IEMap[A, B] =
      new IEMap[A, B] {
        override def toOrFail(a: A): EitherNel[String, B] = _toOrFail(a)
        override def from(b: B): A = _from(b)
      }
  }

}
