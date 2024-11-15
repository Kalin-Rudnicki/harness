package harness.deriving

import cats.syntax.option.*
import cats.syntax.traverse.*
import scala.compiletime.*
import scala.deriving.Mirror
import scala.reflect.ClassTag

abstract class K0T[UB] {

  type Kind[C, O <: UB] = C {
    type MirroredType = O
    type MirroredMonoType = O
    type MirroredElemTypes <: Tuple
  }

  type Generic[O <: UB] = Kind[Mirror, O]
  type ProductGeneric[O <: UB] = Kind[Mirror.Product, O]
  type SumGeneric[O <: UB] = Kind[Mirror.Sum, O]

  implicit class ProductGenericOps[F <: UB](val m: ProductGeneric[F]) {

    object instantiate {

      def tuple(tuple: m.MirroredElemTypes): F =
        m.asInstanceOf[Mirror.ProductOf[m.MirroredMonoType]].fromTuple(tuple.asInstanceOf)

      def tupleUnsafe(tuple: Tuple): F =
        m.asInstanceOf[Mirror.ProductOf[m.MirroredMonoType]].fromTuple(tuple.asInstanceOf)

      def seqUnsafe(seq: Seq[Any]): F =
        tupleUnsafe(Tuple.fromArray(seq.toArray[Any]))

    }

  }

  type FlatFieldInstances[T <: Tuple, F[_ <: UB]] <: Tuple =
    T match {
      case EmptyTuple => EmptyTuple
      case x *: xs    => F[x] *: FlatFieldInstances[xs, F]
    }

  type FieldInstances[T <: Tuple, W[_], F[_ <: UB]] <: Tuple =
    T match {
      case EmptyTuple => EmptyTuple
      case x *: xs    => W[F[x]] *: FieldInstances[xs, W, F]
    }

  inline def summonFlatFieldInstances[T <: Tuple, G[_ <: UB]]: List[G[UB]] =
    summonAll[FlatFieldInstances[T, G]].toIArray.toList.asInstanceOf[List[G[UB]]]

  inline def summonFieldInstances[T <: Tuple, F[_], G[_ <: UB]]: List[F[G[UB]]] =
    summonAll[FieldInstances[T, F, G]].toIArray.toList.asInstanceOf[List[F[G[UB]]]]

  final class ProductInstances[F <: UB, T[_ <: UB]](val m: ProductGeneric[F])(
      val ev: m.MirroredMonoType <:< Product,
      val rawInstances: List[LazyDerived[T[UB]]],
  ) {

    lazy val instances: List[T[UB]] = rawInstances.map(_.derived)

    final case class withInstance(a: F) {

      private def productElements: List[UB] = ev(a).productIterator.toList.asInstanceOf[List[UB]]

      object map {

        def apply[B](f: [t <: UB] => (T[t], t) => B): List[B] =
          productElements.zip(instances).map { case (b, i) => f(i, b) }

        def withLabels[B](labels: Labelling[F])(f: [t <: UB] => (String, T[t], t) => B): List[B] =
          labels.elemLabels.zip(productElements).zip(instances).map { case ((l, b), i) => f(l, i, b) }

      }

      object foldLeft {

        def apply[R](z: R)(f: [t <: UB] => (R, T[t], t) => R): R =
          productElements.zip(instances).foldLeft(z) { case (acc, (b, i)) => f(acc, i, b) }

        def withLabels[R](labels: Labelling[F], z: R)(f: [t <: UB] => (R, String, T[t], t) => R): R =
          labels.elemLabels.zip(productElements).zip(instances).foldLeft(z) { case (acc, ((l, b), i)) => f(acc, l, i, b) }

      }

    }

    object withoutInstance {

      object foldLeft {

        def apply[R](z: R)(f: [t <: UB] => (R, T[t]) => R): R =
          instances.foldLeft(z) { case (acc, i) => f(acc, i) }

        def withLabels[R](labels: Labelling[F], z: R)(f: [t <: UB] => (R, String, T[t]) => R): R =
          labels.elemLabels.zip(instances).foldLeft(z) { case (acc, (l, i)) => f(acc, l, i) }

      }

      object mapInstantiate {

        def apply(f: [t <: UB] => T[t] => t): F =
          m.instantiate.seqUnsafe(instances.map { i => f(i) })

        def withLabels(labels: Labelling[F])(f: [t <: UB] => (String, T[t]) => t): F =
          m.instantiate.seqUnsafe(labels.elemLabels.zip(instances).map { case (l, i) => f(l, i) })

      }

      object mapInstantiateEither {

        def apply[L](f: [t <: UB] => T[t] => Either[L, t]): Either[L, F] =
          instances.traverse { i => f(i) }.map(m.instantiate.seqUnsafe)

        def withLabels[L](labels: Labelling[F])(f: [t <: UB] => (String, T[t]) => Either[L, t]): Either[L, F] =
          labels.elemLabels.zip(instances).traverse { case (l, i) => f(l, i) }.map(m.instantiate.seqUnsafe)

      }

    }

  }
  object ProductInstances {
    inline given of[F <: UB, T[_ <: UB]](using m: ProductGeneric[F]): ProductInstances[F, T] =
      new ProductInstances[F, T](m)(
        summonInline[m.MirroredMonoType <:< Product],
        summonFieldInstances[m.MirroredElemTypes, LazyDerived, T],
      )
  }

  final class SumInstances[F <: UB, T[_ <: UB]](val m: SumGeneric[F])(
      val children: List[T[UB]],
  ) {

    def narrow[G[B <: UB] <: T[B]](implicit fCt: ClassTag[T[m.MirroredType]], gCt: ClassTag[G[m.MirroredType]]): SumInstances[F, G] =
      new SumInstances[F, G](m)(
        children.asInstanceOf[List[Matchable]].map {
          case gCt(c) => c.asInstanceOf[G[UB]]
          case other  => throw new RuntimeException(s"Unable to narrow ${fCt.runtimeClass.getName} to ${gCt.runtimeClass.getName} ($other)")
        },
      )

    final case class withInstance(a: F) {
      val ord: Int = m.ordinal(a)

      object use {

        def apply[B](f: [t <: F] => (i: T[t], t: t) => B): B =
          f[F](children(ord).asInstanceOf[T[F]], a)

        def withLabels[B](labels: Labelling[F])(f: [t <: F] => (l: String, i: T[t], t: t) => B): B =
          f(labels.elemLabels(ord), children(ord).asInstanceOf, a.asInstanceOf)

      }

    }

    def instanceFromLabels(labels: Labelling[F], label: String): Option[T[F]] =
      labels.elemLabels.indexOf(label) match {
        case -1 => None
        case i  => children(i).asInstanceOf[T[F]].some
      }

  }
  object SumInstances {
    inline given of[A <: UB, F[_ <: UB]](using m: SumGeneric[A]): SumInstances[A, F] =
      new SumInstances[A, F](m)(
        summonFieldInstances[m.MirroredElemTypes, Derived, F].map(_.derived),
      )
  }

  trait Derivable[T[_ <: UB]] {

    inline implicit def genProduct[F <: UB](implicit m: ProductGeneric[F]): Derived[T[F]]

    inline implicit def genSum[F <: UB](implicit m: SumGeneric[F]): Derived[T[F]]

    inline final def summonOrDerive[F <: UB](using m: Generic[F]): T[F] =
      compiletime.summonFrom {
        case t: T[F] => t
        case _       => derived[F]
      }

    inline final def derived[F <: UB](using m: Generic[F]): T[F] =
      inline m match {
        case m: ProductGeneric[F] => genProduct[F](using m).derived
        case m: SumGeneric[F]     => genSum[F](using m).derived
      }

  }

}

object K0 extends K0T[Any] {

  type UnionGeneric[A] = UnionMirror[A]
  type IntersectionGeneric[A] = IntersectionMirror[A]

  trait DerivableUnion[T[_]] {

    protected inline def foldUnion[A, B](ta: T[A], tb: T[B]): T[A | B]

    final inline def deriveUnion[A](implicit ev: UnionGeneric[A]): T[A] =
      summonFlatFieldInstances[ev.ElementTypes, T]
        .reduceLeft { (a, b) => foldUnion(a.asInstanceOf[T[Any]], b.asInstanceOf[T[Any]]) }
        .asInstanceOf[T[A]]

  }
  object DerivableUnion {

    trait Auto[T[_]] extends DerivableUnion[T] {
      final inline implicit def deriveUnionAuto[A](implicit ev: UnionGeneric[A]): T[A] = deriveUnion[A]
    }

  }

  trait DerivableIntersection[T[_]] {

    protected inline def foldIntersection[A, B](ta: T[A], tb: T[B]): T[A & B]

    final inline def deriveIntersection[A](implicit ev: IntersectionGeneric[A]): T[A] =
      summonFlatFieldInstances[ev.ElementTypes, T]
        .reduceLeft { (a, b) => foldIntersection(a.asInstanceOf[T[Any]], b.asInstanceOf[T[Any]]) }
        .asInstanceOf[T[A]]

  }
  object DerivableIntersection {

    trait Auto[T[_]] extends DerivableIntersection[T] {
      final inline implicit def deriveIntersectionAuto[A](implicit ev: IntersectionGeneric[A]): T[A] = deriveIntersection[A]
    }

  }

}
