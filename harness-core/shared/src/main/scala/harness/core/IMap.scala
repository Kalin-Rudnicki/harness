package harness.core

import cats.data.EitherNel
import cats.syntax.either.*

trait IMap[A, B] extends IEMap[A, B] {
  def to(a: A): B
  def from(b: B): A
  override final def toOrFail(a: A): EitherNel[String, B] = to(a).asRight
}
object IMap {

  def make[A]: Builder1[A] = new Builder1

  final class Builder1[A] private[IMap] {
    def apply[B](to: A => B): Builder2[A, B] = Builder2(to)
  }

  final class Builder2[A, B] private[IMap] (_to: A => B) {
    def apply(_from: B => A): IMap[A, B] =
      new IMap[A, B] {
        override def to(a: A): B = _to(a)
        override def from(b: B): A = _from(b)
      }
  }

}
