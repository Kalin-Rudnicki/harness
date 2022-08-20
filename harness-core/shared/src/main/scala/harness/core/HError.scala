package harness.core

import cats.syntax.option.*
import scala.annotation.targetName

sealed abstract class HError(
    final val userMessage: String,
    final val internalMessage: String,
    final val cause: Option[Throwable],
) extends Throwable {

  final lazy val fullInternalMessage: String =
    HError.throwableMessage(this, false)

  final lazy val fullInternalMessageWithTrace: String =
    HError.throwableMessage(this, true)

  override final def getMessage: String = userMessage

  override final def toString: String = fullInternalMessage

}
object HError {

  // =====| Error Types |=====

  /**
    * Used when the user of the program provides invalid input.
    */
  final class UserError private (userMessage: String, internalMessage: String, cause: Option[Throwable])
      extends HError(
        userMessage,
        s"The user provided invalid data:\n$internalMessage",
        cause,
      )
  object UserError extends ErrorBuilder3[UserError](new UserError(_, _, _))

  /**
    * Used when you want to start the error message with "WTF, why is this happening"
    * - Map#get(_).thisShouldAlwaysBeThere
    * - List#toNel.thisShouldAlwaysBeNonEmpty
    */
  final class InternalDefect private (internalMessage: String, cause: Option[Throwable])
      extends HError(
        genericUserMessage,
        s"Internal Defect - unexpected failure:\n$internalMessage",
        cause,
      )
  object InternalDefect extends ErrorBuilder2[InternalDefect](new InternalDefect(_, _))

  /**
    * Used when there is an error with the system.
    * - IO
    * - Database
    */
  final class SystemFailure private (internalMessage: String, cause: Option[Throwable])
      extends HError(
        genericUserMessage,
        internalMessage,
        cause,
      )
  object SystemFailure extends ErrorBuilder2[SystemFailure](new SystemFailure(_, _))

  /**
    * Analogous to the scala predef '???', except instead of throwing, it gives you a concrete error type.
    */
  final class ??? private (functionalityVerb: String)
      extends HError(
        s"The ability to '$functionalityVerb' is not yet supported",
        s"Encountered an unimplemented block of code: '$functionalityVerb'",
        None,
      )
  object ??? {
    def apply(functionalityVerb: String): ??? = new ???(functionalityVerb)
  }

  // =====| Helpers |=====

  // TODO (KR) : Come up with a better name & message?
  private val genericUserMessage: String = "There was a non-handleable error in the system"

  private def throwableMessage(throwable: Throwable, stackTrace: Boolean): String = {
    val parts: (String, List[Option[(String, String)]]) =
      throwable match {
        case hError: HError =>
          (
            s"HError[${hError.getClass.getSimpleName}]",
            List(
              ("User Message", hError.userMessage).some,
              Option.when(hError.internalMessage != hError.userMessage)(("Internal Message", hError.internalMessage)),
              hError.cause.map { c => ("Cause", throwableMessage(c, stackTrace)) },
              Option.when(stackTrace)(("Stack Trace", formatExceptionTrace(hError.getStackTrace))),
            ),
          )
        case throwable =>
          (
            throwable.getClass.getSimpleName,
            List(
              Option(throwable.getMessage).map(("Message", _)),
              Option(throwable.getCause).map { c => ("Cause", throwableMessage(c, stackTrace)) },
              Option.when(stackTrace)(("Stack Trace", formatExceptionTrace(throwable.getStackTrace))),
            ),
          )
      }

    parts._2.flatten
      .map { (label, message) => message.split("\n").mkString(s"\n  $label:\n      > ", "\n        ", "") }
      .mkString(s"--- ${parts._1} ---", "", "")
  }

  private def formatExceptionTrace(trace: Array[StackTraceElement]): String = trace.mkString("\n")

  // =====| Builders |=====

  sealed abstract class ErrorBuilder2[E](build: (String, Option[Throwable]) => E) {
    def apply(internalMessage: String): E = build(internalMessage, None)
    def apply(internalMessage: String, cause: Throwable): E = build(internalMessage, cause.some)
    def apply(internalMessage: String, cause: Option[Throwable]): E = build(internalMessage, cause)
  }

  sealed abstract class ErrorBuilder3[E](build: (String, String, Option[Throwable]) => E) {
    def apply(userMessage: String): E = build(userMessage, userMessage, None)
    def apply(userMessage: String, cause: Throwable): E = build(userMessage, userMessage, cause.some)
    def apply(userMessage: String, cause: Option[Throwable]): E = build(userMessage, userMessage, cause)

    def apply(userMessage: String, internalMessage: String): E = build(userMessage, internalMessage, None)
    def apply(userMessage: String, internalMessage: String, cause: Throwable): E = build(userMessage, internalMessage, cause.some)
    def apply(userMessage: String, internalMessage: String, cause: Option[Throwable]): E = build(userMessage, internalMessage, cause)
  }

}
