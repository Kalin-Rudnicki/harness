package harness.zio.test.mock.error

import cats.data.NonEmptyList
import harness.zio.test.mock.*
import harness.zio.test.mock.Types.*

sealed trait MockError extends Throwable {

  // TODO (KR) : colorize?
  override final def getMessage: String = this match {
    case MockError.UnexpectedCall(givenCapability, expectations) =>
      val expStr = expectations match {
        case Nil => "\n    (expected no more calls)"
        case _   => s"\n    expected calls:${expectations.map(e => s"\n      - ${e.name}").mkString}"
      }
      s"\n  Unexpected call to capability ${givenCapability.name}$expStr"
    case MockError.UnsatisfiedCalls(expectations) =>
      s"\n  Unsatisfied seeded calls (test can not exit with remaining expectations):${expectations.toList.map(e => s"\n    - ${e.name}").mkString}"
    case MockError.OverlappingCapabilityImplementations(capabilities) =>
      s"\n  Conflicting capabilities provided for:${capabilities.toList.map(c => s"\n    - ${c.name}").mkString}"
    case MockError.CanNotSeedImplementedCapability(capability) =>
      s"\n  Can not seed capability which is implemented: ${capability.name}"
  }

  override final def toString: String = getMessage

}
object MockError {

  final case class UnexpectedCall(
      givenCapability: ErasedCapability,
      expectations: List[ErasedCapability],
  ) extends MockError

  final case class UnsatisfiedCalls(
      expectations: NonEmptyList[ErasedCapability],
  ) extends MockError

  final case class OverlappingCapabilityImplementations(
      capabilities: NonEmptyList[ErasedCapability],
  ) extends MockError

  final case class CanNotSeedImplementedCapability(
      capability: ErasedCapability,
  ) extends MockError

}
