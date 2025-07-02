package harness.sql

final class OptionEv[T] private {}
object OptionEv {
  given op: [T] => OptionEv[Option[T]] = new OptionEv
}
