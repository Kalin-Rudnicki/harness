package harness.core

extension [A](self: List[A]) {

  def intersperse(value: A): List[A] = self match
    case h :: t => h :: t.flatMap { v => value :: v :: Nil }
    case Nil    => Nil

  def interject(value: A): List[A] =
    value :: self.flatMap { v => v :: value :: Nil }

}
