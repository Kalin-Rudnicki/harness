package harness.deriving

import scala.compiletime.*
import scala.deriving.Mirror
import scala.reflect.ClassTag

object K0 {

  type Instances1[T <: Tuple, F[_]] <: Tuple =
    T match {
      case EmptyTuple => EmptyTuple
      case x *: xs    => F[x] *: Instances1[xs, F]
    }
  type Instances2[T <: Tuple, F[_], G[_]] <: Tuple =
    T match {
      case EmptyTuple => EmptyTuple
      case x *: xs    => F[G[x]] *: Instances2[xs, F, G]
    }

  inline def summonInstances1[T <: Tuple, F[_]]: List[F[Any]] =
    summonAll[Instances1[T, F]].toIArray.toList.asInstanceOf[List[F[Any]]]
  inline def summonInstances2[T <: Tuple, F[_], G[_]]: List[F[G[Any]]] =
    summonAll[Instances2[T, F, G]].toIArray.toList.asInstanceOf[List[F[G[Any]]]]

  final case class ProductInstances[A, F[_]](
      mirror: Mirror.ProductOf[A],
      ev: A <:< Product,
      rawInstances: List[LazyDerived[F[Any]]],
  ) {

    lazy val instances: List[F[Any]] = rawInstances.map(_.derived)

    final case class withInstance(a: A) {

      object map {

        def apply[B](f: [t] => (F[t], t) => B): List[B] =
          ev(a).productIterator.toList.zip(instances).map { case (b, i) => f(i, b) }

        def withLabels[B](labels: Labelling[A])(f: [t] => (String, F[t], t) => B): List[B] =
          labels.elemLabels.zip(ev(a).productIterator.toList).zip(instances).map { case ((l, b), i) => f(l, i, b) }

      }

      object foldLeft {

        def apply[R](z: R)(f: [t] => (R, F[t], t) => R): R =
          ev(a).productIterator.toList.zip(instances).foldLeft(z) { case (acc, (b, i)) => f(acc, i, b) }

        def withLabels[R](labels: Labelling[A], z: R)(f: [t] => (R, String, F[t], t) => R): R =
          labels.elemLabels.zip(ev(a).productIterator.toList).zip(instances).foldLeft(z) { case (acc, ((l, b), i)) => f(acc, l, i, b) }

      }

    }

    object withoutInstance {

      object foldLeft {

        def apply[R](z: R)(f: [t] => (R, F[t]) => R): R =
          instances.foldLeft(z) { case (acc, i) => f(acc, i) }

        def withLabels[R](labels: Labelling[A], z: R)(f: [t] => (R, String, F[t]) => R): R =
          labels.elemLabels.zip(instances).foldLeft(z) { case (acc, (l, i)) => f(acc, l, i) }

      }

    }

  }
  object ProductInstances {
    inline given of[A, F[_]](using m: Mirror.ProductOf[A]): ProductInstances[A, F] =
      ProductInstances[A, F](
        m,
        summonInline[A <:< Product],
        summonAll[Instances2[m.MirroredElemTypes, LazyDerived, F]].toIArray.toList.asInstanceOf[List[LazyDerived[F[Any]]]],
      )
  }

  // TODO (KR) : does sum need to be lazy as well?
  final case class SumInstances[A, F[_]](
      mirror: Mirror.SumOf[A],
      children: List[F[Any]],
  ) {

    def narrow[G[B] <: F[B]](implicit fCt: ClassTag[F[Any]], gCt: ClassTag[G[Any]]): SumInstances[A, G] =
      SumInstances[A, G](
        mirror,
        children.asInstanceOf[List[Matchable]].map {
          case gCt(c) => c
          case other  => throw new RuntimeException(s"Unable to narrow ${fCt.runtimeClass.getName} to ${gCt.runtimeClass.getName} ($other)")
        },
      )

    final case class withInstance(a: A) {
      val ord: Int = mirror.ordinal(a)
      val inst: F[a.type] = children(ord).asInstanceOf
    }

  }
  object SumInstances {
    inline given of[A, F[_]](using m: Mirror.SumOf[A]): SumInstances[A, F] =
      SumInstances[A, F](
        m,
        summonAll[Instances2[m.MirroredElemTypes, Derived, F]].toIArray.toList.asInstanceOf[List[Derived[F[Any]]]].map(_.derived),
      )
  }

  trait Derivable[F[_]] {

    inline implicit def genProduct[A](implicit m: Mirror.ProductOf[A]): Derived[F[A]]

    inline implicit def genSum[A](implicit m: Mirror.SumOf[A]): Derived[F[A]]

    inline final def derive[A](using m: Mirror.Of[A]): F[A] =
      inline m match {
        case m: Mirror.ProductOf[A] => genProduct[A](using m).derived
        case m: Mirror.SumOf[A]     => genSum[A](using m).derived
      }

  }

}
