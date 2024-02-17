package harness.core

extension [A](self: Option[A]) {

  def cata[B](none: => B, some: A => B): B =
    self match {
      case Some(o) => some(o)
      case None    => none
    }

}

extension [A, B](self: Either[A, B]) {

  def cata[C](left: A => C, right: B => C): C =
    self match {
      case Right(value) => right(value)
      case Left(value)  => left(value)
    }

}
