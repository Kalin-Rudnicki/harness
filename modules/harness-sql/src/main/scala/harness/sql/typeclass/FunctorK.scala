package harness.sql.typeclass

import harness.deriving.*
import harness.deriving.K11.~>

trait FunctorK[T[_[_]]] {
  def mapK[A[_], B[_]](t: T[A])(f: A ~> B): T[B]
}
object FunctorK {

  inline def apply[T[_[_]]](using fh: FunctorK[T]): FunctorK[T] = fh

  implicit def id[T]: FunctorK[K11.Id[T]] =
    new FunctorK[K11.Id[T]] {
      override def mapK[A[_], B[_]](t: K11.Id[T][A])(f: A ~> B): K11.Id[T][B] = f(t)
    }

  @scala.annotation.nowarn
  inline def derive[T[_[_]]](implicit inst: K11.ProductGeneric[T]): FunctorK[T] = {
    val insts = K11.ProductInstances.of[T, FunctorK]

    new FunctorK[T] {
      override def mapK[A[_], B[_]](t: T[A])(f: A ~> B): T[B] =
        insts.withInstance(t).mapInstantiate { [t[_[_]]] => (i: FunctorK[t], t: t[A]) => i.mapK[A, B](t)(f) }
    }
  }

}
