package harness.deriving

import cats.syntax.option.*
import scala.compiletime.*
import scala.deriving.Mirror
import scala.reflect.ClassTag

abstract class K1T[UB] {

  type Kind[C, O[_ <: UB]] = C {
    type MirroredType[X <: UB] = O[X]
    type MirroredMonoType = O[UB]
    type MirroredElemTypes[_ <: UB] <: Tuple
  }

  type Generic[O[_ <: UB]] = Kind[Mirror, O]
  type ProductGeneric[O[_ <: UB]] = Kind[Mirror.Product, O]
  type SumGeneric[O[_ <: UB]] = Kind[Mirror.Sum, O]

  type Id[x <: UB] = x

  type Head[T <: [_ <: UB] =>> Any, A] =
    T[A] match {
      case h *: ? => h
    }
  type Tail[T <: [_ <: UB] =>> Any, A] =
    T[A] match {
      case ? *: t => t
    }

  type FieldInstances[T <: [_ <: UB] =>> Tuple, F[_], G[_[_ <: UB]]] <: Tuple =
    T[UB] match {
      case ? *: ?     => F[G[[X] =>> Head[T, X]]] *: FieldInstances[[X] =>> Tail[T, X], F, G]
      case EmptyTuple => EmptyTuple
    }

  inline def summonFieldInstances[T <: [_ <: UB] =>> Tuple, F[_], G[_[_ <: UB]]]: List[F[G[[_ <: UB] =>> Any]]] =
    summonAll[FieldInstances[T, F, G]].toIArray.toList.asInstanceOf[List[F[G[[_ <: UB] =>> Any]]]]

  final class ProductInstances[F[_ <: UB], T[_[_ <: UB]]](val m: ProductGeneric[F])(
      val ev: m.MirroredMonoType <:< Product,
      val rawInstances: List[LazyDerived[T[[_ <: UB] =>> Any]]],
  ) {

    lazy val instances: List[T[[_ <: UB] =>> Any]] = rawInstances.map(_.derived)

    def instantiate[A <: UB](fields: List[m.MirroredType[A]]): F[A] =
      m.asInstanceOf[Mirror.ProductOf[m.MirroredMonoType]].fromTuple(Tuple.fromArray(fields.toArray[Any]).asInstanceOf).asInstanceOf[F[A]]

    final case class withInstance[A <: UB](a: F[A]) {

      private def productElements: List[T[[_ <: UB] =>> Any]] = ev(a.asInstanceOf).productIterator.toList.asInstanceOf[List[T[[_ <: UB] =>> Any]]]

      object map {

        def apply[B](f: [t[_ <: UB]] => (T[t], t[A]) => B): List[B] =
          productElements.zip(instances).map { case (b, i) => f(i, b) }

        def withLabels[B](labels: Labelling[m.MirroredMonoType])(f: [t[_ <: UB]] => (String, T[t], t[A]) => B): List[B] =
          labels.elemLabels.zip(productElements).zip(instances).map { case ((l, b), i) => f(l, i, b) }

      }

      object mapInstantiate {

        def apply[B <: UB](f: [t[_ <: UB]] => (T[t], t[A]) => t[B]): F[B] =
          instantiate(map(f).asInstanceOf)

        def withLabels[B <: UB](labels: Labelling[m.MirroredMonoType])(f: [t[_ <: UB]] => (String, T[t], t[A]) => t[B]): F[B] =
          instantiate(map.withLabels(labels)(f).asInstanceOf)

      }

      object foldLeft {

        def apply[R](z: R)(f: [t[_ <: UB]] => (R, T[t], t[A]) => R): R =
          productElements.zip(instances).foldLeft(z) { case (acc, (b, i)) => f(acc, i, b) }

        def withLabels[R](labels: Labelling[m.MirroredMonoType], z: R)(f: [t[_ <: UB]] => (R, String, T[t], t[A]) => R): R =
          labels.elemLabels.zip(productElements).zip(instances).foldLeft(z) { case (acc, ((l, b), i)) => f(acc, l, i, b) }

      }

    }

    object withoutInstance {

      object foldLeft {

        def apply[R](z: R)(f: [t[_ <: UB]] => (R, T[t]) => R): R =
          instances.foldLeft(z) { case (acc, i) => f(acc, i) }

        def withLabels[R](labels: Labelling[m.MirroredMonoType], z: R)(f: [t[_ <: UB]] => (R, String, T[t]) => R): R =
          labels.elemLabels.zip(instances).foldLeft(z) { case (acc, (l, i)) => f(acc, l, i) }

      }

    }

  }
  object ProductInstances {
    inline given of[F[_ <: UB], T[_[_ <: UB]]](using m: ProductGeneric[F]): ProductInstances[F, T] =
      new ProductInstances[F, T](m)(
        summonInline[m.MirroredMonoType <:< Product],
        summonFieldInstances[m.MirroredElemTypes, LazyDerived, T],
      )
  }

  final class SumInstances[F[_ <: UB], T[_[_ <: UB]]](val m: SumGeneric[F])(
      children: List[T[[_ <: UB] =>> Any]],
  ) {

    // TODO (KR) : Im not sure on this one...
    def narrow[G[B[_ <: UB]] <: T[B]](implicit fCt: ClassTag[T[m.MirroredType]], gCt: ClassTag[G[m.MirroredType]]): SumInstances[F, G] =
      new SumInstances[F, G](m)(
        children.asInstanceOf[List[Matchable]].map {
          case gCt(c) => c.asInstanceOf
          case other  => throw new RuntimeException(s"Unable to narrow ${fCt.runtimeClass.getName} to ${gCt.runtimeClass.getName} ($other)")
        },
      )

    final case class withInstance[A <: UB](a: F[A]) {
      val ord: Int = m.ordinal(a.asInstanceOf)

      object use {

        def apply[B](f: [t[C <: UB] <: F[C]] => (T[t], t[A]) => B): B =
          f[F](children(ord).asInstanceOf[T[F]], a)

        def withLabels[B](labels: Labelling[m.MirroredMonoType])(f: [t[C <: UB] <: F[C]] => (String, T[t], t[A]) => B): B =
          f[F](labels.elemLabels(ord), children(ord).asInstanceOf[T[F]], a)

      }

    }

    def instanceFromLabels(labels: Labelling[m.MirroredMonoType], label: String): Option[T[F]] =
      labels.elemLabels.indexOf(label) match {
        case -1 => None
        case i  => children(i).asInstanceOf[T[F]].some
      }

  }
  object SumInstances {
    inline given of[F[_ <: UB], T[_[_ <: UB]]](using m: SumGeneric[F]): SumInstances[F, T] =
      new SumInstances[F, T](m)(
        summonFieldInstances[m.MirroredElemTypes, Derived, T].map(_.derived),
      )
  }

  trait Derivable[T[_[_ <: UB]]] {

    inline implicit def genProduct[F[_ <: UB]](implicit m: ProductGeneric[F]): Derived[T[F]]

    inline implicit def genSum[F[_ <: UB]](implicit m: SumGeneric[F]): Derived[T[F]]

    inline final def derive[F[_ <: UB]](using m: Generic[F]): T[F] =
      inline m match {
        case m: ProductGeneric[F] => genProduct[F](using m).derived
        case m: SumGeneric[F]     => genSum[F](using m).derived
      }

  }

}

object K1 extends K1T[Any]
