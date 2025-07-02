package harness.deriving

import cats.syntax.option.*
import scala.compiletime.*
import scala.deriving.Mirror
import scala.reflect.ClassTag

abstract class K11T[UB] {

  type Kind[C, O[_[_ <: UB]]] = C {
    type MirroredType[X[_ <: UB]] = O[X]
    type MirroredMonoType = O[[_ <: UB] =>> Any]
    type MirroredElemTypes[_[_ <: UB]] <: Tuple
  }

  type Generic[O[_[_ <: UB]]] = Kind[Mirror, O]
  type ProductGeneric[O[_[_ <: UB]]] = Kind[Mirror.Product, O]
  type SumGeneric[O[_[_ <: UB]]] = Kind[Mirror.Sum, O]

  implicit class ProductGenericOps[F[_[_ <: UB]]](val m: ProductGeneric[F]) {

    object instantiate {

      def tuple[A[_ <: UB]](tuple: m.MirroredElemTypes[A]): F[A] =
        m.asInstanceOf[Mirror.ProductOf[m.MirroredMonoType]].fromTuple(tuple.asInstanceOf).asInstanceOf[F[A]]

      def tupleUnsafe[A[_ <: UB]](tuple: Tuple): F[A] =
        m.asInstanceOf[Mirror.ProductOf[m.MirroredMonoType]].fromTuple(tuple.asInstanceOf).asInstanceOf[F[A]]

      def seqUnsafe[A[_ <: UB]](seq: Seq[Any]): F[A] =
        tupleUnsafe(Tuple.fromArray(seq.toArray[Any]))

    }

  }
  type Id[t <: UB] = [f[_ <: UB]] =>> f[t]
  type Identity[t <: UB] = t
  type Const[c] = [_ <: UB] =>> c
  type ~>[A[_ <: UB], B[_ <: UB]] = [t <: UB] => A[t] => B[t]
  type Zip[A[_ <: UB], B[_ <: UB]] = [t <: UB] =>> (A[t], B[t])

  type Head[T <: [_[_ <: UB]] =>> Any, A[_ <: UB]] =
    T[A] match {
      case h *: ? => h
    }
  type Tail[T <: [_[_ <: UB]] =>> Any, A[_ <: UB]] =
    T[A] match {
      case ? *: t => t
    }

  type FieldInstances[T <: [_[_ <: UB]] =>> Tuple, F[_], G[_[_[_ <: UB]]]] <: Tuple =
    T[Option] match {
      case ? *: ?     => F[G[[X[_ <: UB]] =>> Head[T, X]]] *: FieldInstances[[X[_ <: UB]] =>> Tail[T, X], F, G]
      case EmptyTuple => EmptyTuple
    }

  extension [O[_[_ <: UB]]](self: ProductGeneric[O]) {

    inline def toRepr[A[_ <: UB]](a: O[A]): self.MirroredElemTypes[A] =
      Tuple.fromProduct(summonInline[O[A] <:< Product].apply(a)).asInstanceOf[self.MirroredElemTypes[A]]

    inline def fromRepr[A[_ <: UB]](a: self.MirroredElemTypes[A]): O[A] = {
      val p = self.asInstanceOf[Mirror.ProductOf[self.MirroredElemTypes[A]]]
      p.fromTuple(a.asInstanceOf[p.MirroredElemTypes]).asInstanceOf[O[A]]
    }

  }

  inline def summonFieldInstances[T <: [_[_ <: UB]] =>> Tuple, F[_], G[_[_[_ <: UB]]]]: List[F[G[[_[_ <: UB]] =>> Any]]] =
    summonAll[FieldInstances[T, F, G]].toIArray.toList.asInstanceOf[List[F[G[[_[_ <: UB]] =>> Any]]]]

  final class ProductInstances[F[_[_ <: UB]], T[_[_[_ <: UB]]]](val m: ProductGeneric[F])(
      val ev: m.MirroredMonoType <:< Product,
      val rawInstances: List[LazyDerived[T[[_[_ <: UB]] =>> Any]]],
  ) {

    lazy val instances: List[T[[_[_ <: UB]] =>> Any]] = rawInstances.map(_.derived)

    object withoutInstance {

      object foldLeft {

        def apply[R](z: R)(f: [t[_[_ <: UB]]] => (R, T[t]) => R): R =
          instances.foldLeft(z) { case (acc, i) => f(acc, i) }

        def withLabels[R](labels: Labelling[m.MirroredMonoType], z: R)(f: [t[_[_ <: UB]]] => (R, String, T[t]) => R): R =
          labels.elemLabels.zip(instances).foldLeft(z) { case (acc, (l, i)) => f(acc, l, i) }

      }

    }

    final case class withInstance[A[_ <: UB]](a: F[A]) {

      private def productElements: List[T[[_[_ <: UB]] =>> Any]] = ev(a.asInstanceOf).productIterator.toList.asInstanceOf[List[T[[_[_ <: UB]] =>> Any]]]

      object map {

        def apply[B](f: [t[_[_ <: UB]]] => (T[t], t[A]) => B): List[B] =
          productElements.zip(instances).map { case (b, i) => f(i, b) }

        def withLabels[B](labels: Labelling[m.MirroredMonoType])(f: [t[_[_ <: UB]]] => (String, T[t], t[A]) => B): List[B] =
          labels.elemLabels.zip(productElements).zip(instances).map { case ((l, b), i) => f(l, i, b) }

      }

      object mapInstantiate {

        def apply[B[_ <: UB]](f: [t[_[_ <: UB]]] => (T[t], t[A]) => t[B]): F[B] =
          m.instantiate.seqUnsafe(map(f))

        def withLabels[B[_ <: UB]](labels: Labelling[m.MirroredMonoType])(f: [t[_[_ <: UB]]] => (String, T[t], t[A]) => t[B]): F[B] =
          m.instantiate.seqUnsafe(map.withLabels(labels)(f))

      }

      object foldLeft {

        def apply[R](z: R)(f: [t[_[_ <: UB]]] => (R, T[t], t[A]) => R): R =
          productElements.zip(instances).foldLeft(z) { case (acc, (b, i)) => f(acc, i, b) }

        def withLabels[R](labels: Labelling[m.MirroredMonoType], z: R)(f: [t[_[_ <: UB]]] => (R, String, T[t], t[A]) => R): R =
          labels.elemLabels.zip(productElements).zip(instances).foldLeft(z) { case (acc, ((l, b), i)) => f(acc, l, i, b) }

      }

    }

    final case class withInstance2[A[_ <: UB], B[_ <: UB]](a: F[A], b: F[B]) {

      private def productElementsA: List[T[[_[_ <: UB]] =>> Any]] = ev(a.asInstanceOf).productIterator.toList.asInstanceOf[List[T[[_[_ <: UB]] =>> Any]]]
      private def productElementsB: List[T[[_[_ <: UB]] =>> Any]] = ev(b.asInstanceOf).productIterator.toList.asInstanceOf[List[T[[_[_ <: UB]] =>> Any]]]

      object map {

        def apply[C](f: [t[_[_ <: UB]]] => (T[t], t[A], t[B]) => C): List[C] =
          productElementsA.zip(productElementsB).zip(instances).map { case ((a, b), i) => f(i, a, b) }

        def withLabels[C](labels: Labelling[m.MirroredMonoType])(f: [t[_[_ <: UB]]] => (String, T[t], t[A], t[B]) => C): List[C] =
          labels.elemLabels.zip(productElementsA.zip(productElementsB)).zip(instances).map { case ((l, (a, b)), i) => f(l, i, a, b) }

      }

      object mapInstantiate {

        def apply[C[_ <: UB]](f: [t[_[_ <: UB]]] => (T[t], t[A], t[B]) => t[C]): F[C] =
          m.instantiate.seqUnsafe(map(f))

        def withLabels[C[_ <: UB]](labels: Labelling[m.MirroredMonoType])(f: [t[_[_ <: UB]]] => (String, T[t], t[A], t[B]) => t[C]): F[C] =
          m.instantiate.seqUnsafe(map.withLabels(labels)(f))

      }

    }

  }
  object ProductInstances {
    inline given of: [F[_[_ <: UB]], T[_[_[_ <: UB]]]] => (m: ProductGeneric[F]) => ProductInstances[F, T] =
      new ProductInstances[F, T](m)(
        summonInline[m.MirroredMonoType <:< Product],
        summonFieldInstances[m.MirroredElemTypes, LazyDerived, T],
      )
  }

  final class SumInstances[F[_[_ <: UB]], T[_[_[_ <: UB]]]](val m: SumGeneric[F])(
      children: List[T[[_[_ <: UB]] =>> Any]],
  ) {

    // TODO (KR) : Im not sure on this one...
    def narrow[G[B[_[_ <: UB]]] <: T[B]](implicit fCt: ClassTag[T[m.MirroredType]], gCt: ClassTag[G[m.MirroredType]]): SumInstances[F, G] =
      new SumInstances[F, G](m)(
        children.asInstanceOf[List[Matchable]].map {
          case gCt(c) => c.asInstanceOf
          case other  => throw new RuntimeException(s"Unable to narrow ${fCt.runtimeClass.getName} to ${gCt.runtimeClass.getName} ($other)")
        },
      )

    final case class withInstance[A[_ <: UB]](a: F[A]) {
      val ord: Int = m.ordinal(a.asInstanceOf)

      object use {

        def apply[B](f: [t[C[_ <: UB]] <: F[C]] => (T[t], t[A]) => B): B =
          f[F](children(ord).asInstanceOf[T[F]], a)

        def withLabels[B](labels: Labelling[m.MirroredMonoType])(f: [t[C[_ <: UB]] <: F[C]] => (String, T[t], t[A]) => B): B =
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
    inline given of: [F[_[_ <: UB]], T[_[_[_ <: UB]]]] => (m: SumGeneric[F]) => SumInstances[F, T] =
      new SumInstances[F, T](m)(
        summonFieldInstances[m.MirroredElemTypes, Derived, T].map(_.derived),
      )
  }

  trait Derivable[T[_[_[_ <: UB]]]] {

    inline implicit def genProduct[F[_[_ <: UB]]](implicit m: ProductGeneric[F]): Derived[T[F]]

    inline implicit def genSum[F[_[_ <: UB]]](implicit m: SumGeneric[F]): Derived[T[F]]

    inline final def derived[F[_[_ <: UB]]](using m: Generic[F]): T[F] =
      inline m match {
        case m: ProductGeneric[F] => genProduct[F](using m).derived
        case m: SumGeneric[F]     => genSum[F](using m).derived
      }

  }

}

object K11 extends K11T[Any]
