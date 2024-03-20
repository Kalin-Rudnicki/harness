package harness.zio.mock

import cats.syntax.list.*
import harness.zio.mock.Types.*
import harness.zio.mock.error.MockError
import zio.*

final class Proxy private (
    private val impls: Map[ErasedCapability, ErasedEffectImpl],
    private val expectationsRef: Ref[List[ErasedExpectation]],
) {

  def apply[Z, I, R, E, A](
      capability: Mock[Z]#Capability[I, R, E, A],
      i: I,
  ): ZIO[R, E, A] =
    impls.get(capability) match {
      case Some(effectImpl) =>
        val typed: EffectImpl[I, R, E, A] = effectImpl.asInstanceOf
        typed(i)
      case None =>
        expectationsRef
          .modify {
            case head :: tail if head.capability == capability =>
              val result: EffectImpl[I, R, E, A] = head.effect.asInstanceOf
              (result, tail)
            case seededExpectations =>
              (
                _ => ZIO.die(MockError.UnexpectedCall(capability, seededExpectations.map(_.capability))),
                seededExpectations,
              )
          }
          .flatMap(_(i))
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

  private[mock] def prependSeed[Z, I, R, E, A](capability: Mock[Z]#Capability[I, R, E, A], effect: I => ZIO[R, E, A]): UIO[Unit] =
    ZIO.die(MockError.CanNotSeedImplementedCapability(capability)).when(impls.contains(capability)) *>
      expectationsRef.update(Expectation(capability, effect) :: _)

  private[mock] def appendSeed[Z, I, R, E, A](capability: Mock[Z]#Capability[I, R, E, A], effect: I => ZIO[R, E, A]): UIO[Unit] =
    ZIO.die(MockError.CanNotSeedImplementedCapability(capability)).when(impls.contains(capability)) *>
      expectationsRef.update(_ :+ Expectation(capability, effect))

}
object Proxy {

  private def makeScoped(impls: Map[ErasedCapability, ErasedEffectImpl]): URIO[Scope, Proxy] =
    Ref.make(List.empty[ErasedExpectation]).map { new Proxy(impls, _) }.withFinalizer {
      _.expectationsRef.get.flatMap {
        _.toNel match {
          case Some(unsatisfiedExpectations) => ZIO.die(MockError.UnsatisfiedCalls(unsatisfiedExpectations.map(_.capability)))
          case None                          => ZIO.unit
        }
      }
    }

  private[mock] def make(impls: Map[ErasedCapability, ErasedEffectImpl]): ULayer[Proxy] =
    ZLayer.scoped { makeScoped(impls) }

}
