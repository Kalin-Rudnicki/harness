package harness.deriving

object ExampleK11Types {

  // =====| Simple |=====

  final case class ProductSimple[F[_]](
      string: F[String],
      int: F[Int],
      boolean: F[Boolean],
      optString: F[Option[String]],
      optInt: F[Option[Int]],
      optBoolean: F[Option[Boolean]],
  )

  sealed trait SumSimple[F[_]]
  object SumSimple {

    final case class Case1[F[_]](
        string: F[String],
        int: F[Int],
        boolean: F[Boolean],
    ) extends SumSimple[F]

    final case class Case2[F[_]](
        optString: F[Option[String]],
        optInt: F[Option[Int]],
        optBoolean: F[Option[Boolean]],
    ) extends SumSimple[F]

    final case class Case3[F[_]](
        product1: ProductSimple[F],
        product2: F[ProductSimple[F]],
    )

  }

}
