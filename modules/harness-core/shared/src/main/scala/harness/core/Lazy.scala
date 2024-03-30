package harness.core

final class Lazy[A](_value: => A) {
  lazy val value: A = _value
  override def toString: String = s"Lazy($value)"
}
object Lazy {
  def apply[A](value: => A): Lazy[A] = new Lazy[A](value)
}
