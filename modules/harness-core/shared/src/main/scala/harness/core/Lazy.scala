package harness.core

final class Lazy[A](_value: => A) {

  lazy val value: A = _value

  def map[B](f: A => B): Lazy[B] = Lazy(f(value))
  def flatMap[B](f: A => Lazy[B]): Lazy[B] = Lazy(f(value)).flatten

  def flatten[B](implicit ev: A <:< Lazy[B]): Lazy[B] = Lazy(ev(value).value)

  override def toString: String = s"Lazy($value)"

}
object Lazy {
  def apply[A](value: => A): Lazy[A] = new Lazy[A](value)
}
