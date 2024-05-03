package harness.zio.mock

import cats.syntax.list.*
import harness.zio.mock.Types.*
import harness.zio.mock.error.MockError
import zio.*

final class Proxy private (
    private val implsRef: Ref[Map[ErasedCapability, ErasedEffectImpl]],
    private val expectationsRef: Ref[List[Proxy.Seed]],
) {

  def apply[Z, I, R, E, A](
      capability: Mock[Z]#Capability[I, R, E, A],
      i: I,
  ): ZIO[R, E, A] = {
    object forCapability {
      def unapply(map: Map[ErasedCapability, ErasedExpectation]): Option[(EffectImpl[I, R, E, A], Option[Map[ErasedCapability, ErasedExpectation]])] =
        map.get(capability).map { exp =>
          val effect: EffectImpl[I, R, E, A] = exp.effect.asInstanceOf
          val newMap = map.removed(capability)
          (effect, Option.when(newMap.nonEmpty)(newMap))
        }
    }

    for {
      impl <- implsRef.get.map(_.get(capability))
      effect <- expectationsRef.modify {
        case Proxy.Seed.Sync(head) :: tail if head.capability == capability =>
          val result: EffectImpl[I, R, E, A] = head.effect.asInstanceOf
          (result, tail)
        case Proxy.Seed.Async(forCapability(result, newExps)) :: tail =>
          (result, newExps.fold(tail)(exps => Proxy.Seed.Async(exps) :: tail))
        case seededExpectations =>
          impl match {
            case Some(effectImpl) =>
              val typed: EffectImpl[I, R, E, A] = effectImpl.asInstanceOf
              (
                typed,
                seededExpectations,
              )
            case None =>
              (
                (_: I) => ZIO.die(MockError.UnexpectedCall(capability, seededExpectations)),
                seededExpectations,
              )
          }
      }
      result <- effect(i)
    } yield result
  }

  def apply[Z, I, R, E, A](
      capability: Mock[Z]#Capability[I, R, E, A],
  )(implicit ev: Unit <:< I): ZIO[R, E, A] =
    apply(capability, ev(()))

  def apply[Z, I1, I2, R, E, A](
      capability: Mock[Z]#Capability[(I1, I2), R, E, A],
      i1: I1,
      i2: I2,
  ): ZIO[R, E, A] =
    apply(capability, (i1, i2))

  def apply[Z, I1, I2, I3, R, E, A](
      capability: Mock[Z]#Capability[(I1, I2, I3), R, E, A],
      i1: I1,
      i2: I2,
      i3: I3,
  ): ZIO[R, E, A] =
    apply(capability, (i1, i2, i3))

  def apply[Z, I1, I2, I3, I4, R, E, A](
      capability: Mock[Z]#Capability[(I1, I2, I3, I4), R, E, A],
      i1: I1,
      i2: I2,
      i3: I3,
      i4: I4,
  ): ZIO[R, E, A] =
    apply(capability, (i1, i2, i3, i4))

  def apply[Z, I1, I2, I3, I4, I5, R, E, A](
      capability: Mock[Z]#Capability[(I1, I2, I3, I4, I5), R, E, A],
      i1: I1,
      i2: I2,
      i3: I3,
      i4: I4,
      i5: I5,
  ): ZIO[R, E, A] =
    apply(capability, (i1, i2, i3, i4, i5))

  def apply[Z, I1, I2, I3, I4, I5, I6, R, E, A](
      capability: Mock[Z]#Capability[(I1, I2, I3, I4, I5, I6), R, E, A],
      i1: I1,
      i2: I2,
      i3: I3,
      i4: I4,
      i5: I5,
      i6: I6,
  ): ZIO[R, E, A] =
    apply(capability, (i1, i2, i3, i4, i5, i6))

  def apply[Z, I1, I2, I3, I4, I5, I6, I7, R, E, A](
      capability: Mock[Z]#Capability[(I1, I2, I3, I4, I5, I6, I7), R, E, A],
      i1: I1,
      i2: I2,
      i3: I3,
      i4: I4,
      i5: I5,
      i6: I6,
      i7: I7,
  ): ZIO[R, E, A] =
    apply(capability, (i1, i2, i3, i4, i5, i6, i7))

  def apply[Z, I1, I2, I3, I4, I5, I6, I7, I8, R, E, A](
      capability: Mock[Z]#Capability[(I1, I2, I3, I4, I5, I6, I7, I8), R, E, A],
      i1: I1,
      i2: I2,
      i3: I3,
      i4: I4,
      i5: I5,
      i6: I6,
      i7: I7,
      i8: I8,
  ): ZIO[R, E, A] =
    apply(capability, (i1, i2, i3, i4, i5, i6, i7, i8))

  def apply[Z, I1, I2, I3, I4, I5, I6, I7, I8, I9, R, E, A](
      capability: Mock[Z]#Capability[(I1, I2, I3, I4, I5, I6, I7, I8, I9), R, E, A],
      i1: I1,
      i2: I2,
      i3: I3,
      i4: I4,
      i5: I5,
      i6: I6,
      i7: I7,
      i8: I8,
      i9: I9,
  ): ZIO[R, E, A] =
    apply(capability, (i1, i2, i3, i4, i5, i6, i7, i8, i9))

  def apply[Z, I1, I2, I3, I4, I5, I6, I7, I8, I9, I10, R, E, A](
      capability: Mock[Z]#Capability[(I1, I2, I3, I4, I5, I6, I7, I8, I9, I10), R, E, A],
      i1: I1,
      i2: I2,
      i3: I3,
      i4: I4,
      i5: I5,
      i6: I6,
      i7: I7,
      i8: I8,
      i9: I9,
      i10: I10,
  ): ZIO[R, E, A] =
    apply(capability, (i1, i2, i3, i4, i5, i6, i7, i8, i9, i10))

  // =====|  |=====

  private[mock] def putImpl[Z, I, R, E, A](capability: Mock[Z]#Capability[I, R, E, A], effect: I => ZIO[R, E, A]): UIO[Unit] =
    ZIO.die(MockError.CapabilityIsAlreadyImplemented(capability)).whenZIO(implsRef.get.map(_.contains(capability))) *>
      implsRef.update(_.updated(capability, effect))

  private[mock] def prependSeed[Z, I, R, E, A](capability: Mock[Z]#Capability[I, R, E, A], effect: I => ZIO[R, E, A]): UIO[Unit] =
    expectationsRef.update(Proxy.Seed.Sync(Expectation(capability, effect)) :: _)

  private[mock] def appendSeed[Z, I, R, E, A](capability: Mock[Z]#Capability[I, R, E, A], effect: I => ZIO[R, E, A]): UIO[Unit] =
    expectationsRef.update(_ :+ Proxy.Seed.Sync(Expectation(capability, effect)))

  private[mock] def appendAsyncSeed[Z, I, R, E, A](capability: Mock[Z]#Capability[I, R, E, A], effect: I => ZIO[R, E, A]): UIO[Unit] = {
    val expectation: ErasedExpectation = Expectation(capability, effect)
    expectationsRef.modify { expectations =>
      expectations.reverse match {
        case Proxy.Seed.Async(exps) :: tail =>
          if (exps.contains(expectation.capability)) (ZIO.die(MockError.DuplicateAsyncSeed(capability)), expectations)
          else (ZIO.unit, (Proxy.Seed.Async(exps.updated(expectation.capability, expectation)) :: tail).reverse)
        case rev =>
          (ZIO.unit, (Proxy.Seed.Async(Map(expectation.capability -> expectation)) :: rev).reverse)
      }
    }.flatten
  }

  private val appendEmptyAsync: UIO[Unit] =
    expectationsRef.update(_ :+ Proxy.Seed.Async(Map.empty))

}
object Proxy {

  private val makeWithoutFinalizer: UIO[Proxy] =
    for {
      implsRef <- Ref.make(Map.empty[ErasedCapability, ErasedEffectImpl])
      expectationsRef <- Ref.make(List.empty[Seed])
    } yield new Proxy(implsRef, expectationsRef)

  private val makeWithFinalizer: URIO[Scope, Proxy] =
    makeWithoutFinalizer.withFinalizer {
      _.expectationsRef.get.flatMap {
        _.toNel match {
          case Some(unsatisfiedExpectations) => ZIO.die(MockError.UnsatisfiedCalls(unsatisfiedExpectations))
          case None                          => ZIO.unit
        }
      }
    }

  val layer: ULayer[Proxy] =
    ZLayer.scoped { makeWithFinalizer }

  val seedEmptyAsync: URIO[Proxy, Unit] =
    ZIO.serviceWithZIO[Proxy](_.appendEmptyAsync)

  sealed trait Seed {

    final def capabilities: Set[ErasedCapability] = this match {
      case Seed.Sync(expectation)   => Set(expectation.capability)
      case Seed.Async(expectations) => expectations.keySet
    }

    final def hasCapability(capability: ErasedCapability): Boolean = this match {
      case Seed.Sync(expectation)   => expectation.capability == capability
      case Seed.Async(expectations) => expectations.contains(capability)
    }

  }
  object Seed {
    final case class Sync(expectation: ErasedExpectation) extends Seed
    final case class Async(expectations: Map[ErasedCapability, ErasedExpectation]) extends Seed
  }

}
