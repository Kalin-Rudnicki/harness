package harness.zio.mock.error

import cats.data.NonEmptyList
import harness.zio.mock.*
import harness.zio.mock.Types.*

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
    case MockError.CapabilityIsAlreadyImplemented(capability) =>
      s"\n  Capability is already implemented: ${capability.name}"
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

  final case class CapabilityIsAlreadyImplemented(
      capability: ErasedCapability,
  ) extends MockError

}
