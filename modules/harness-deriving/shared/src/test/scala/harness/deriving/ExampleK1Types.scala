package harness.deriving

object ExampleK1Types {

  // =====| Simple |=====

  final case class ProductSimple[+A](
      option: Option[A],
      list: List[A],
  )

  sealed trait SumSimple[+A]
  object SumSimple {
    final case class Case1[+A](option: Option[A]) extends SumSimple[A]
    final case class Case2[+A](list: List[A]) extends SumSimple[A]
    final case class Case3[+A](product: ProductSimple[A]) extends SumSimple[A]
    final case class Case4() extends SumSimple[Nothing]
  }

}
