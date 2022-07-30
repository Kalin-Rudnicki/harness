package harness.cli

final case class Indexed[+V](value: V, idx: Int) {
  def map[V2](f: V => V2): Indexed[V2] = Indexed(f(value), idx)
}
