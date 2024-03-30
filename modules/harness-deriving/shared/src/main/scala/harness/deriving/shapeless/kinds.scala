package harness.deriving.shapeless

import scala.Tuple.Union
import scala.deriving.*
import shapeless3.deriving.*
import shapeless3.deriving.internals.*

abstract class K1T[LB]:
  type Kind[C, O[_ <: LB]] = C {
    type Kind = K1.type
    type MirroredType[X <: LB] = O[X]
    type MirroredMonoType = O[LB]
    type MirroredElemTypes[_ <: LB] <: Tuple
  }

  type Generic[O[_ <: LB]] = Kind[Mirror, O]
  type ProductGeneric[O[_ <: LB]] = Kind[Mirror.Product, O]
  type CoproductGeneric[O[_ <: LB]] = Kind[Mirror.Sum, O]

  def Generic[O[_ <: LB]](using gen: Generic[O]): gen.type = gen
  def ProductGeneric[O[_ <: LB]](using gen: ProductGeneric[O]): gen.type = gen
  def CoproductGeneric[O[_ <: LB]](using gen: CoproductGeneric[O]): gen.type = gen

  type Instances[F[_[_ <: LB]], T[_ <: LB]] = ErasedInstances[K1.type, F[T]]
  type ProductInstances[F[_[_ <: LB]], T[_ <: LB]] = ErasedProductInstances[K1.type, F[T]]
  type CoproductInstances[F[_[_ <: LB]], T[_ <: LB]] = ErasedCoproductInstances[K1.type, F[T]]

  type InstancesOf[F[_[_ <: LB]]] = [T[_ <: LB]] =>> Instances[F, T]
  type ProductInstancesOf[F[_[_ <: LB]]] = [T[_ <: LB]] =>> ProductInstances[F, T]
  type CoproductInstancesOf[F[_[_ <: LB]]] = [T[_ <: LB]] =>> CoproductInstances[F, T]

  def Instances[F[_[_ <: LB]], T[_ <: LB]](using inst: Instances[F, T]): inst.type = inst
  def ProductInstances[F[_[_ <: LB]], T[_ <: LB]](using
      inst: ProductInstances[F, T],
  ): inst.type = inst
  def CoproductInstances[F[_[_ <: LB]], T[_ <: LB]](using
      inst: CoproductInstances[F, T],
  ): inst.type = inst

  type Head[T <: [X <: LB] =>> Any, A] = T[A] match
    case h *: t => h
  type Tail[T <: [X <: LB] =>> Any, A] = T[A] match
    case h *: t => t

  type LiftP[F[_[_ <: LB]], T <: [X <: LB] =>> Any] <: Tuple =
    T[Any] match
      case ? *: ? => F[[X] =>> Head[T, X]] *: LiftP[F, [X] =>> Tail[T, X]]
      case _      => EmptyTuple

  /**
    * Summon the first given instance `F[U]` from the tuple `T`. Remaining elements of `T` may or may not have an
    * instance of `F`.
    */
  inline def summonFirst[F[_[_ <: LB]], T[_ <: LB]]: F[[_] =>> Any] =
    Kinds.summonFirst[LiftP[F, T]].asInstanceOf[F[[_] =>> Any]]

  @deprecated("Use summonFirst instead", "3.2.0")
  transparent inline def summonFirst0[T]: Any =
    Kinds.summonFirst[T]

  /**
    * Summon the only given instance `F[U]` from the tuple `T`. Remaining elements of `T` are guaranteed to not have an
    * instance of `F`.
    */
  inline def summonOnly[F[_[_ <: LB]], T[_ <: LB]]: F[[_] =>> Any] =
    Kinds.summonOnly[LiftP[F, T]].asInstanceOf[F[[_] =>> Any]]

  /** Ensure that no element of the tuple `T` has an instance of `F`. */
  inline def summonNone[F[_[_ <: LB]], T[_ <: LB]]: Unit =
    Kinds.summonNone[LiftP[F, T]]

  extension [T[_ <: LB], A <: LB](gen: ProductGeneric[T])
    inline def toRepr(o: T[A]): gen.MirroredElemTypes[A] =
      Tuple.fromProduct(o.asInstanceOf).asInstanceOf[gen.MirroredElemTypes[A]]
    inline def fromRepr(r: gen.MirroredElemTypes[A]): T[A] = gen.fromProduct(r.asInstanceOf).asInstanceOf[T[A]]

  extension [T[_ <: LB], A <: LB](gen: CoproductGeneric[T])
    inline def toRepr(o: T[A]): Union[gen.MirroredElemTypes[A]] = o.asInstanceOf
    inline def fromRepr(r: Union[gen.MirroredElemTypes[A]]): T[A] = r.asInstanceOf
    inline def withFirst[F[_[_ <: LB]], R](f: [t[x <: LB] <: T[x]] => F[t] => R): R = f(
      summonFirst[F, gen.MirroredElemTypes].asInstanceOf,
    )
    inline def withOnly[F[_[_ <: LB]], R](f: [t[x <: LB] <: T[x]] => F[t] => R): R = f(
      summonOnly[F, gen.MirroredElemTypes].asInstanceOf,
    )

  extension [F[_[_ <: LB]], T[_ <: LB]](gen: Generic[T])
    inline def derive(
        f: => (ProductGeneric[T] & gen.type) ?=> F[T],
        g: => (CoproductGeneric[T] & gen.type) ?=> F[T],
    ): F[T] =
      inline gen match
        case p: ProductGeneric[T]   => f(using p.asInstanceOf)
        case c: CoproductGeneric[T] => g(using c.asInstanceOf)

  extension [F[_[_ <: LB]], T[_ <: LB]](inst: Instances[F, T])
    inline def mapK[G[_[_ <: LB]]](f: [t[_ <: LB]] => F[t] => G[t]): Instances[G, T] =
      inst.erasedMapK(f.asInstanceOf).asInstanceOf
    inline def map[A <: LB, R <: LB](x: T[A])(
        f: [t[_ <: LB]] => (F[t], t[A]) => t[R],
    ): T[R] =
      inst.erasedMap(x)(f.asInstanceOf).asInstanceOf
    inline def widen[G[t[_ <: LB]] >: F[t]]: Instances[G, T] =
      inst.asInstanceOf

  extension [F[_[_ <: LB]], T[_ <: LB]](inst: ProductInstances[F, T])
    inline def mapK[G[_[_ <: LB]]](f: [t[_ <: LB]] => F[t] => G[t]): ProductInstances[G, T] =
      inst.erasedMapK(f.asInstanceOf).asInstanceOf
    inline def construct[R <: LB](f: [t[_ <: LB]] => F[t] => t[R]): T[R] =
      inst.erasedConstruct(f.asInstanceOf).asInstanceOf
    inline def map2[A <: LB, B <: LB, R <: LB](x: T[A], y: T[B])(
        f: [t[_ <: LB]] => (F[t], t[A], t[B]) => t[R],
    ): T[R] =
      inst.erasedMap2(x, y)(f.asInstanceOf).asInstanceOf
    inline def foldLeft[A <: LB, Acc](x: T[A])(i: Acc)(
        f: [t[_ <: LB]] => (Acc, F[t], t[A]) => CompleteOr[Acc],
    ): Acc =
      inst.erasedFoldLeft(x)(i)(f.asInstanceOf).asInstanceOf
    inline def foldLeft2[A <: LB, B <: LB, Acc](x: T[A], y: T[B])(i: Acc)(
        f: [t[_ <: LB]] => (Acc, F[t], t[A], t[B]) => CompleteOr[Acc],
    ): Acc =
      inst.erasedFoldLeft2(x, y)(i)(f.asInstanceOf).asInstanceOf
    inline def foldRight[A <: LB, Acc](x: T[A])(i: Acc)(
        f: [t[_ <: LB]] => (F[t], t[A], Acc) => CompleteOr[Acc],
    ): Acc =
      inst.erasedFoldRight(x)(i)(f.asInstanceOf).asInstanceOf
    inline def foldRight2[A <: LB, B <: LB, Acc](x: T[A], y: T[B])(i: Acc)(
        f: [t[_ <: LB]] => (F[t], t[A], t[B], Acc) => CompleteOr[Acc],
    ): Acc =
      inst.erasedFoldRight2(x, y)(i)(f.asInstanceOf).asInstanceOf
    inline def project[A <: LB, R](t: T[A])(p: Int)(f: [t[_ <: LB]] => (F[t], t[A]) => R): R =
      inst.erasedProject(t)(p)(f.asInstanceOf).asInstanceOf
    inline def widen[G[t[_ <: LB]] >: F[t]]: ProductInstances[G, T] =
      inst.asInstanceOf

  extension [F[_[_ <: LB]], T[_ <: LB]](inst: CoproductInstances[F, T])
    inline def mapK[G[_[_ <: LB]]](
        f: [t[x <: LB] <: T[x]] => F[t] => G[t],
    ): CoproductInstances[G, T] =
      inst.erasedMapK(f.asInstanceOf).asInstanceOf
    inline def fold[A <: LB, R](x: T[A])(f: [t[x <: LB] <: T[x]] => (F[t], t[A]) => R): R =
      inst.erasedFold(x)(f.asInstanceOf).asInstanceOf
    inline def fold2[A <: LB, B <: LB, R](x: T[A], y: T[B])(a: => R)(
        f: [t[x <: LB] <: T[x]] => (F[t], t[A], t[B]) => R,
    ): R =
      inst.erasedFold2(x, y)(a.asInstanceOf)(f.asInstanceOf).asInstanceOf
    inline def fold2[A <: LB, B <: LB, R](x: T[A], y: T[B])(g: (Int, Int) => R)(
        f: [t[x <: LB] <: T[x]] => (F[t], t[A], t[B]) => R,
    ): R =
      inst.erasedFold2f(x, y)(g.asInstanceOf)(f.asInstanceOf).asInstanceOf
    inline def widen[G[t[_ <: LB]] >: F[t]]: CoproductInstances[G, T] =
      inst.asInstanceOf

  inline given mkInstances[F[_[_ <: LB]], T[_ <: LB]](using
      gen: Generic[T],
  ): Instances[F, T] =
    inline gen match
      case p: ProductGeneric[T]   => mkProductInstances[F, T](using p)
      case c: CoproductGeneric[T] => mkCoproductInstances[F, T](using c)

  inline given mkProductInstances[F[_[_ <: LB]], T[_ <: LB]](using
      gen: ProductGeneric[T],
  ): ProductInstances[F, T] =
    ErasedProductInstances[K1.type, F[T], LiftP[F, gen.MirroredElemTypes]](gen)

  inline given mkCoproductInstances[F[_[_ <: LB]], T[_ <: LB]](using
      gen: CoproductGeneric[T],
  ): CoproductInstances[F, T] =
    ErasedCoproductInstances[K1.type, F[T], LiftP[F, gen.MirroredElemTypes]](gen)

abstract class K11T[LB]:
  type Kind[C, O[_[_ <: LB]]] = C {
    type Kind = K11.type
    type MirroredType[X[_ <: LB]] = O[X]
    type MirroredMonoType = O[[_ <: LB] =>> Any]
    type MirroredElemTypes[_[_ <: LB]] <: Tuple
  }

  type Generic[O[_[_ <: LB]]] = Kind[Mirror, O]
  type ProductGeneric[O[_[_ <: LB]]] = Kind[Mirror.Product, O]
  type CoproductGeneric[O[_[_ <: LB]]] = Kind[Mirror.Sum, O]

  def Generic[O[_[_ <: LB]]](using gen: Generic[O]): gen.type = gen
  def ProductGeneric[O[_[_ <: LB]]](using gen: ProductGeneric[O]): gen.type = gen
  def CoproductGeneric[O[_[_ <: LB]]](using gen: CoproductGeneric[O]): gen.type = gen

  type Instances[F[_[_[_ <: LB]]], T[_[_ <: LB]]] = ErasedInstances[K11.type, F[T]]
  type ProductInstances[F[_[_[_ <: LB]]], T[_[_ <: LB]]] =
    ErasedProductInstances[K11.type, F[T]]
  type CoproductInstances[F[_[_[_ <: LB]]], T[_[_ <: LB]]] =
    ErasedCoproductInstances[K11.type, F[T]]

  type InstancesOf[F[_[_[_ <: LB]]]] = [T[_[_ <: LB]]] =>> Instances[F, T]
  type ProductInstancesOf[F[_[_[_ <: LB]]]] = [T[_[_ <: LB]]] =>> ProductInstances[F, T]
  type CoproductInstancesOf[F[_[_[_ <: LB]]]] = [T[_[_ <: LB]]] =>> CoproductInstances[F, T]

  def Instances[F[_[_[_ <: LB]]], T[_[_ <: LB]]](using inst: Instances[F, T]): inst.type =
    inst
  def ProductInstances[F[_[_[_ <: LB]]], T[_[_ <: LB]]](using
      inst: ProductInstances[F, T],
  ): inst.type = inst
  def CoproductInstances[F[_[_[_ <: LB]]], T[_[_ <: LB]]](using
      inst: CoproductInstances[F, T],
  ): inst.type = inst

  type Id[t <: LB] = [f[_ <: LB]] =>> f[t]
  type Const[c <: LB] = [f[_ <: LB]] =>> c

  type Head[T <: [G[_ <: LB]] =>> Any, A[_ <: LB]] = T[A] match
    case h *: t => h
  type Tail[T <: [G[_ <: LB]] =>> Any, A[_ <: LB]] = T[A] match
    case h *: t => t

  type LiftP[F[_[_[_ <: LB]]], T <: [G[_ <: LB]] =>> Any] <: Tuple =
    T[Option] match
      case ? *: ? => F[[A[_ <: LB]] =>> Head[T, A]] *: LiftP[F, [A[_]] =>> Tail[T, A]]
      case _      => EmptyTuple

  /**
    * Summon the first given instance `F[U]` from the tuple `T`. Remaining elements of `T` may or may not have an
    * instance of `F`.
    */
  inline def summonFirst[F[_[_[_ <: LB]]], T[_[_ <: LB]]]: F[[_[_ <: LB]] =>> Any] =
    Kinds.summonFirst[LiftP[F, T]].asInstanceOf[F[[_[_ <: LB]] =>> Any]]

  @deprecated("Use summonFirst instead", "3.2.0")
  transparent inline def summonFirst0[T]: Any =
    Kinds.summonFirst[T]

  /**
    * Summon the only given instance `F[U]` from the tuple `T`. Remaining elements of `T` are guaranteed to not have an
    * instance of `F`.
    */
  inline def summonOnly[F[_[_[_ <: LB]]], T[_[_ <: LB]]]: F[[_[_ <: LB]] =>> Any] =
    Kinds.summonOnly[LiftP[F, T]].asInstanceOf[F[[_[_ <: LB]] =>> Any]]

  /** Ensure that no element of the tuple `T` has an instance of `F`. */
  inline def summonNone[F[_[_[_ <: LB]]], T[_[_ <: LB]]]: Unit =
    Kinds.summonNone[LiftP[F, T]]

  extension [T[_[_ <: LB]], A[_ <: LB]](gen: ProductGeneric[T])
    inline def toRepr(o: T[A]): gen.MirroredElemTypes[A] =
      Tuple.fromProduct(o.asInstanceOf).asInstanceOf[gen.MirroredElemTypes[A]]
    inline def fromRepr(r: gen.MirroredElemTypes[A]): T[A] = gen.fromProduct(r.asInstanceOf).asInstanceOf[T[A]]

  extension [T[_[_ <: LB]], A[_ <: LB]](gen: CoproductGeneric[T])
    inline def toRepr(o: T[A]): Union[gen.MirroredElemTypes[A]] = o.asInstanceOf
    inline def fromRepr(r: Union[gen.MirroredElemTypes[A]]): T[A] = r.asInstanceOf
    inline def withFirst[F[_[_[_]]], R](f: [t[x[_]] <: T[x]] => F[t] => R): R = f(
      summonFirst[F, gen.MirroredElemTypes].asInstanceOf,
    )
    inline def withOnly[F[_[_[_]]], R](f: [t[x[_]] <: T[x]] => F[t] => R): R = f(
      summonOnly[F, gen.MirroredElemTypes].asInstanceOf,
    )

  extension [F[_[_[_ <: LB]]], T[_[_ <: LB]]](gen: Generic[T])
    inline def derive(
        f: => (ProductGeneric[T] & gen.type) ?=> F[T],
        g: => (CoproductGeneric[T] & gen.type) ?=> F[T],
    ): F[T] =
      inline gen match
        case p: ProductGeneric[T]   => f(using p.asInstanceOf)
        case c: CoproductGeneric[T] => g(using c.asInstanceOf)

  extension [F[_[_[_ <: LB]]], T[_[_ <: LB]]](inst: Instances[F, T])
    inline def mapK[G[_[_[_ <: LB]]]](f: [t[_[_ <: LB]]] => F[t] => G[t]): Instances[G, T] =
      inst.erasedMapK(f.asInstanceOf).asInstanceOf
    inline def map[A[_ <: LB], R[_ <: LB]](x: T[A])(
        f: [t[_[_ <: LB]]] => (F[t], t[A]) => t[R],
    ): T[R] =
      inst.erasedMap(x)(f.asInstanceOf).asInstanceOf
    inline def widen[G[t[_[_ <: LB]]] >: F[t]]: Instances[G, T] =
      inst.asInstanceOf

  extension [F[_[_[_ <: LB]]], T[_[_ <: LB]]](inst: ProductInstances[F, T])
    inline def mapK[G[_[_[_ <: LB]]]](
        f: [t[_[_ <: LB]]] => F[t] => G[t],
    ): ProductInstances[G, T] =
      inst.erasedMapK(f.asInstanceOf).asInstanceOf
    inline def construct[R[_ <: LB]](f: [t[_[_ <: LB]]] => F[t] => t[R]): T[R] =
      inst.erasedConstruct(f.asInstanceOf).asInstanceOf
    inline def map2[A[_ <: LB], B[_ <: LB], R[_ <: LB]](x: T[A], y: T[B])(
        f: [t[_[_ <: LB]]] => (F[t], t[A], t[B]) => t[R],
    ): T[R] =
      inst.erasedMap2(x, y)(f.asInstanceOf).asInstanceOf
    inline def foldLeft[A[_ <: LB], Acc](x: T[A])(i: Acc)(
        f: [t[_[_ <: LB]]] => (Acc, F[t], t[A]) => CompleteOr[Acc],
    ): Acc =
      inst.erasedFoldLeft(x)(i)(f.asInstanceOf).asInstanceOf
    inline def foldLeft2[A[_ <: LB], B[_ <: LB], Acc](x: T[A], y: T[B])(i: Acc)(
        f: [t[_[_ <: LB]]] => (Acc, F[t], t[A], t[B]) => CompleteOr[Acc],
    ): Acc =
      inst.erasedFoldLeft2(x, y)(i)(f.asInstanceOf).asInstanceOf
    inline def foldRight[A[_ <: LB], Acc](x: T[A])(i: Acc)(
        f: [t[_[_ <: LB]]] => (F[t], t[A], Acc) => CompleteOr[Acc],
    ): Acc =
      inst.erasedFoldRight(x)(i)(f.asInstanceOf).asInstanceOf
    inline def foldRight2[A[_ <: LB], B[_ <: LB], Acc](x: T[A], y: T[B])(i: Acc)(
        f: [t[_[_ <: LB]]] => (F[t], t[A], t[B], Acc) => CompleteOr[Acc],
    ): Acc =
      inst.erasedFoldRight2(x, y)(i)(f.asInstanceOf).asInstanceOf
    inline def project[A[_ <: LB], R](t: T[A])(p: Int)(
        f: [t[_[_ <: LB]]] => (F[t], t[A]) => R,
    ): R =
      inst.erasedProject(t)(p)(f.asInstanceOf).asInstanceOf
    inline def widen[G[t[_[_ <: LB]]] >: F[t]]: ProductInstances[G, T] =
      inst.asInstanceOf

  extension [F[_[_[_ <: LB]]], T[_[_ <: LB]]](inst: CoproductInstances[F, T])
    inline def mapK[G[_[_[_ <: LB]]]](
        f: [t[x[_ <: LB]] <: T[x]] => F[t] => G[t],
    ): CoproductInstances[G, T] =
      inst.erasedMapK(f.asInstanceOf).asInstanceOf
    inline def fold[A[_ <: LB], R](x: T[A])(
        f: [t[x[_ <: LB]] <: T[x]] => (F[t], t[A]) => R,
    ): R =
      inst.erasedFold(x)(f.asInstanceOf).asInstanceOf
    inline def fold2[A[_ <: LB], B[_ <: LB], R](x: T[A], y: T[B])(a: => R)(
        f: [t[x[_ <: LB]] <: T[x]] => (F[t], t[A], t[B]) => R,
    ): R =
      inst.erasedFold2(x, y)(a.asInstanceOf)(f.asInstanceOf).asInstanceOf
    inline def fold2[A[_], B[_], R](x: T[A], y: T[B])(g: (Int, Int) => R)(
        f: [t[x[_ <: LB]] <: T[x]] => (F[t], t[A], t[B]) => R,
    ): R =
      inst.erasedFold2f(x, y)(g.asInstanceOf)(f.asInstanceOf).asInstanceOf
    inline def widen[G[t[_[_ <: LB]]] >: F[t]]: CoproductInstances[G, T] =
      inst.asInstanceOf

  inline given mkInstances[F[_[_[_ <: LB]]], T[_[_ <: LB]]](using
      gen: Generic[T],
  ): Instances[F, T] =
    inline gen match
      case p: ProductGeneric[T]   => mkProductInstances[F, T](using p)
      case c: CoproductGeneric[T] => mkCoproductInstances[F, T](using c)

  inline given mkProductInstances[F[_[_[_ <: LB]]], T[_[_ <: LB]]](using
      gen: ProductGeneric[T],
  ): ProductInstances[F, T] =
    ErasedProductInstances[K11.type, F[T], LiftP[F, gen.MirroredElemTypes]](gen)

  inline given mkCoproductInstances[F[_[_[_ <: LB]]], T[_[_ <: LB]]](using
      gen: CoproductGeneric[T],
  ): CoproductInstances[F, T] =
    ErasedCoproductInstances[K11.type, F[T], LiftP[F, gen.MirroredElemTypes]](gen)
