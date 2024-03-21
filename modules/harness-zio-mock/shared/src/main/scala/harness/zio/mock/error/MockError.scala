package harness.zio.mock.error

import cats.data.NonEmptyList
import harness.zio.mock.*
import harness.zio.mock.Types.*

sealed trait MockError extends Throwable {
  import MockError.{attnStar, hintHeader}

  // TODO (KR) : colorize?
  override final def getMessage: String = {
    val baseMessage: String =
      this match {
        case MockError.UnexpectedCall(givenCapability, expectations) =>
          val seededCallsStr = expectations match {
            case Nil => "\n    (expected no more calls)"
            case _   => expectations.map(e => s"\n    - ${if (e == givenCapability) attnStar else ""}${e.name}").mkString
          }

          expectations.indexOf(givenCapability) match {
            case -1 =>
              s"""Unexpected call to capability ${givenCapability.name}
                 |  currently seeded calls:$seededCallsStr
                 |$hintHeader
                 |  It looks like you have no upcoming seeded calls to ${givenCapability.name}.
                 |  Either your code called this function when you didn't want it to, or you forgot to seed this call.""".stripMargin
            case nextIndex =>
              val unexpected = expectations.take(nextIndex)
              s"""Unexpected call to capability ${givenCapability.name}
                 |  currently seeded calls:$seededCallsStr
                 |$hintHeader
                 |  It looks like you seeded a call to ${givenCapability.name} later on.
                 |  This most likely means that you did not account for the following calls: ${unexpected.map(e => s"\n    - ${e.name}").mkString}
                 |  Either your code called these functions when you didn't want it to, or you forgot to seed these calls.""".stripMargin
          }
        case MockError.UnsatisfiedCalls(expectations) =>
          s"""Unsatisfied seeded calls:${expectations.toList.map(e => s"\n  - ${e.name}").mkString}
             |$hintHeader
             |  Tests can not exit with seeded calls that did not end up being called.
             |  This means you either need to remove calls you didn't expect to actually be called,
             |  or your test is failing, because calls you expected to be made were not made.""".stripMargin
        case MockError.CapabilityIsAlreadyImplemented(capability) =>
          val mockImplStr = s"${capability.getMock.name}.${capability.namePart}.implement.___(___)"
          s"""Capability is already implemented: ${capability.name}
             |$hintHeader
             |  You can not implement the same capability multiple times.
             |  You essentially did $mockImplStr ++ $mockImplStr""".stripMargin
      }

    s"\n$baseMessage".replaceAll("\n", "\n  ")
  }

  override final def toString: String = getMessage

}
object MockError {

  private val attnStar: String = s"${scala.io.AnsiColor.CYAN}(*)${scala.io.AnsiColor.RESET} "
  private val hintHeader: String = s"${scala.io.AnsiColor.MAGENTA}<< HINT >>${scala.io.AnsiColor.RESET}"

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
