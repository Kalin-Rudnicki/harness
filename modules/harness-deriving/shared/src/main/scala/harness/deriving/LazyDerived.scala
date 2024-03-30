package harness.deriving

final class LazyDerived[A] private (_derived: => A) {
  lazy val derived: A = _derived
  override def toString: String = s"LazyDerived($derived)"

}
object LazyDerived {

  def apply[A](a: => A): LazyDerived[A] = new LazyDerived[A](a)

  def apply[A](implicit derived: LazyDerived[A]): LazyDerived[A] = derived

  type K0[F[_]] = [a] =>> LazyDerived[F[a]]

  given inst[A](using a: => A): LazyDerived[A] = LazyDerived(a)

}
