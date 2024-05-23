package harness.sql.query

final case class Constant[T](value: T) {
  def map[T2](f: T => T2): Constant[T2] = Constant(f(value))
}
