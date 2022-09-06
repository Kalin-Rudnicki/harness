package harness.sql

final class OptionEv[T] private {}
object OptionEv {
  implicit def op[A]: OptionEv[A] = new OptionEv[A]
}
