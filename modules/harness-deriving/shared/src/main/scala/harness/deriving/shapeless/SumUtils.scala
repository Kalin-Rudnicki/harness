package harness.deriving.shapeless

import shapeless3.deriving.*

object SumUtils {

  extension [F[_], T](inst: K0.CoproductInstances[F, T]) {

    def coproductInstanceIndexOf(t: T): Int = inst.mirror.ordinal(t.asInstanceOf)

    def coproductInstance(idx: Int): F[T] = inst.is(idx).asInstanceOf[F[T]]
    def coproductInstanceOf(t: T): F[T] = coproductInstance(coproductInstanceIndexOf(t))

    def injectFor[R](_t: T)(f: [t] => (F[t], t) => R): R =
      inst.inject[R](coproductInstanceIndexOf(_t)) { [t] => (i: F[t]) => f[t](i, _t.asInstanceOf[t]) }

  }
}
