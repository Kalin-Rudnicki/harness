package harness.zio.mock.error

import cats.data.NonEmptyList
import cats.syntax.option.*
import harness.zio.mock.*
import harness.zio.mock.Proxy.Seed
import harness.zio.mock.Types.*

sealed trait MockError extends Throwable {
  import MockError.{attnStar, hintHeader}

  // TODO (KR) : colorize?
  override final def getMessage: String = {
    def showSeeds(seeds: List[Proxy.Seed], attn: Option[ErasedCapability]): String =
      seeds.map {
        case Seed.Sync(expectation) =>
          s"\n    - ${if (attn.contains(expectation.capability)) attnStar else ""}${expectation.capability.name}"
        case Seed.Async(expectations) =>
          s"\n    - [Async]:${expectations.values.map(e => s"\n      - ${if (attn.contains(e.capability)) attnStar else ""}${e.capability.name}").mkString}"
      }.mkString

    val baseMessage: String =
      this match {
        case MockError.UnexpectedCall(givenCapability, expectations) =>
          val seededCallsStr = expectations match {
            case Nil => "\n    (expected no more calls)"
            case _   => showSeeds(expectations, givenCapability.some)
          }

          expectations.indexWhere(_.hasCapability(givenCapability)) match {
            case -1 =>
              s"""Unexpected call to capability ${givenCapability.name}
                 |  currently seeded calls:$seededCallsStr
                 |$hintHeader
                 |  It looks like you have no upcoming seeded calls to ${givenCapability.name}.
                 |  Likely causes & solutions:
                 |    1) You do not expect this call to actually happen.
                 |       solution: fix your code such that the call is not made.
                 |    2) You do in deed expect this call to happen.
                 |       solution: your code that is seeding calls is incorrect, add code to seed this expectation.""".stripMargin
            case nextIndex =>
              val unexpected = expectations.take(nextIndex)
              s"""Unexpected call to capability ${givenCapability.name}
                 |  currently seeded calls:$seededCallsStr
                 |$hintHeader
                 |  It looks like you seeded a call to ${givenCapability.name} later on.
                 |  Calls that are expected before that call:${showSeeds(unexpected, None)}
                 |  Likely causes & solutions:
                 |    1) You do in deed expect these other calls to happen first.
                 |       solution: fix your code such that these other calls happen first.
                 |    2) You do in deed expect these calls to be made, and followed by the call that is currently failing, but you also expect that call to happen now.
                 |       solution: your code that is seeding calls is incorrect, add code to seed this expectation first.
                 |    3) You don't expect these other calls to be made.
                 |       solution: your code that is seeding calls is incorrect, remove the code that seeds these other expectations.""".stripMargin
          }
        case MockError.UnsatisfiedCalls(expectations) =>
          s"""Unsatisfied seeded calls:${showSeeds(expectations.toList, None)}
             |$hintHeader
             |  Tests can not exit with seeded calls that did not end up being called.
             |  Likely causes & solutions:
             |    1) You do in deed expect these calls to happen, and they didn't.
             |       solution: fix your code such that these calls are actually made.
             |    2) You do not actually expect these calls to happen.
             |       solution: your code that is seeding calls is incorrect, remove the code that seeds these expectations.""".stripMargin
        case MockError.CapabilityIsAlreadyImplemented(capability) =>
          val mockImplStr = s"${capability.getMock.name}.${capability.namePart}.implement.___(___)"
          s"""Capability is already implemented: ${capability.name}
             |$hintHeader
             |  You can not implement the same capability multiple times.
             |  You essentially did $mockImplStr ++ $mockImplStr""".stripMargin
        case MockError.FailedExpectedSeed(capability, input) =>
          val showInput = (input.asInstanceOf[Matchable] match {
            case tup: Tuple => tup.toList
            case _          => input :: Nil
          }).map(e => s"\n    - $e").mkString
          s"""Capability called with invalid input: ${capability.name}
             |  actual input:$showInput
             |$hintHeader
             |  Either you have an error in your code, or your seeded expectation is asserting the wrong thing.""".stripMargin
        case MockError.DuplicateAsyncSeed(capability) =>
          s"""Duplicate async capability seeded: ${capability.name}
             |$hintHeader
             |  It's not supported to seed multiple async expectations for the same capability.""".stripMargin
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
      expectations: List[Proxy.Seed],
  ) extends MockError

  final case class UnsatisfiedCalls(
      expectations: NonEmptyList[Proxy.Seed],
  ) extends MockError

  final case class CapabilityIsAlreadyImplemented(
      capability: ErasedCapability,
  ) extends MockError

  final case class FailedExpectedSeed(
      capability: ErasedCapability,
      input: Any,
  ) extends MockError

  final case class DuplicateAsyncSeed(
      capability: ErasedCapability,
  ) extends MockError

}
