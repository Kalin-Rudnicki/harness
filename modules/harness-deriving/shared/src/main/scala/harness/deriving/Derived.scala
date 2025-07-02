package harness.deriving

final case class Derived[A](derived: A)
object Derived {

  def apply[A](implicit derived: Derived[A]): Derived[A] = derived

  type K0[F[_]] = [a] =>> Derived[F[a]]

  given inst: [A] => (a: A) => Derived[A] = Derived(a)

}
