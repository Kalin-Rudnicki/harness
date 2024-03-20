package harness.zio.test.mock

import cats.syntax.list.*
import harness.zio.test.mock.Types.*
import harness.zio.test.mock.error.MockError
import zio.*

sealed trait Mocked[Z] { self =>

  final def ++[Z2](that: Mocked[Z2]): Mocked[Z & Z2] =
    (self.asInstanceOf[Mocked[Z & Z2]], that.asInstanceOf[Mocked[Z & Z2]]) match {
      case (self: Mocked.Building[Z & Z2], that: Mocked.Building[Z & Z2]) =>
        (self.impls.keySet & that.impls.keySet).toList.toNel match {
          case None =>
            new Mocked.Building[Z & Z2](
              impls = self.impls ++ that.impls,
              mocks = self.mocks | that.mocks,
            )
          case Some(conflictingCapabilities) =>
            Mocked.FailedDuringBuild(MockError.OverlappingCapabilityImplementations(conflictingCapabilities))
        }
      case (self: Mocked.FailedDuringBuild[Z & Z2], _)                          => self
      case (_: Mocked.Building[Z & Z2], that: Mocked.FailedDuringBuild[Z & Z2]) => that
    }

  final def toLayer(implicit envTag: EnvironmentTag[Z]): ULayer[Proxy & Z] =
    self match {
      case self: Mocked.Building[Z] =>
        Proxy.make(self.impls.asInstanceOf[Map[ErasedCapability, ErasedEffectImpl]]) >+>
          ZLayer.fromZIOEnvironment {
            ZIO.serviceWith[Proxy] { proxy =>
              val zEnvs: List[ZEnvironment[?]] =
                self.mocks.toList.map { _.build(proxy) }
              val zEnv: ZEnvironment[Z] =
                zEnvs.foldLeft(ZEnvironment.empty) { _ ++ _ }.asInstanceOf[ZEnvironment[Z]]

              zEnv
            }
          }
      case Mocked.FailedDuringBuild(error) =>
        ZLayer.die(error)
    }

}
object Mocked {

  // =====| Builders |=====

  private final class Building[Z](
      private[Mocked] val impls: Map[ErasedCapabilityZ[? >: Z], ErasedEffectImpl],
      private[Mocked] val mocks: Set[Mock[? >: Z]],
  ) extends Mocked[Z]

  private final case class FailedDuringBuild[Z](
      error: Throwable,
  ) extends Mocked[Z]

  // =====|  |=====

  private[mock] def single[Z, I, R, E, A](capability: Mock[Z]#Capability[I, R, E, A], effect: I => ZIO[R, E, A]): Mocked[Z] =
    new Mocked.Building[Z](
      Map(capability -> effect),
      Set(capability.getMock),
    )

  private[mock] def empty[Z](mock: Mock[Z]): Mocked[Z] =
    new Mocked.Building[Z](
      Map.empty,
      Set(mock),
    )

}
