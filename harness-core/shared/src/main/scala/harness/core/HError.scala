package harness.core

import cats.data.NonEmptyList
import cats.syntax.option.*
import cats.syntax.traverse.*
import harness.core.RunMode.{Dev, Prod}
import scala.annotation.targetName

sealed abstract class HError(
    final val userMessage: HError.UserMessage,
    final val internalMessage: String,
    final val causes: List[Throwable],
) extends Throwable {

  // TODO (KR) : Tweak these (`baseUserMessage`, `userMessage` = s"${baseUserMessage}\n${causes.flatMap(_.userMessage.toOption}" ...
  //           : and the same for internalMessage
  //           : turn current `fullInternalMessage` into a sort of `verboseInternalMessage`

  final lazy val fullInternalMessage: String =
    HError.throwableMessage(this, false)

  final lazy val fullInternalMessageWithTrace: String =
    HError.throwableMessage(this, true)

  override final def getMessage: String = userMessage.show(HError.UserMessage.IfHidden.default)

  override final def toString: String = fullInternalMessage

  final def toNel: NonEmptyList[HError.Single] = HError.unwrap(this)

  final def formatMessage(runMode: RunMode, ifHidden: HError.UserMessage.IfHidden): String =
    runMode match {
      case Prod => userMessage.show(ifHidden)
      case Dev  => fullInternalMessage // TODO (KR) :
    }

}
object HError {

  def fromThrowable(throwable: Throwable): HError =
    throwable match {
      case hError: HError => hError
      case _              => HError.Wrapped(throwable)
    }

  def unwrap(err: HError): NonEmptyList[HError.Single] =
    err match {
      case HError.Multiple(children) => children
      case err: HError.Single        => NonEmptyList.one(err)
    }

  def apply(err: NonEmptyList[HError]): HError =
    err match {
      case NonEmptyList(err, Nil) => err
      case _                      => HError.Multiple(err.flatMap(HError.unwrap))
    }
  inline def apply(err0: HError, errN: HError*): HError =
    HError(NonEmptyList(err0, errN.toList))

  // =====|  |=====

  trait UserMessage {

    def show(ifHidden: UserMessage.IfHidden): String

    final def toOption: Option[String] =
      this match {
        case UserMessage.Const(msg) => msg.some
        case _                      => None
      }

  }
  object UserMessage {

    final case class Const(msg: String) extends UserMessage {
      override def show(ifHidden: IfHidden): String = msg
    }
    val hidden: UserMessage = _.hiddenMsg

    def fromOption(msg: Option[String]): UserMessage =
      msg match {
        case Some(msg) => UserMessage.Const(msg)
        case None      => UserMessage.hidden
      }

    final case class IfHidden(hiddenMsg: String)
    object IfHidden {
      val default: IfHidden = IfHidden("There was an internal system error")
    }

  }

  // =====| Direct Children of HError |=====

  abstract class Single(
      userMessage: HError.UserMessage,
      internalMessage: String,
      causes: List[Throwable],
  ) extends HError(userMessage, internalMessage, causes)

  final case class Multiple private[HError] (children: NonEmptyList[HError.Single])
      extends HError(
        ifHidden => children.toList.map(e => e.userMessage.show(ifHidden).split("\n").mkString("- ", "  ", "")).mkString("\n"),
        children.toList.map(e => e.internalMessage.split("\n").mkString("- ", "  ", "")).mkString("\n"),
        children.toList.flatMap(_.causes),
      )

  // =====| HError.Single Descendants |=====

  /**
    * Only purpose is to lift a non-HError Throwable into an HError.
    * If another HError uses this as a cause, the wrapping will be removed.
    */
  final case class Wrapped private[HError] (wrapped: Throwable)
      extends HError.Single(
        HError.UserMessage.hidden,
        Option(wrapped.getMessage).getOrElse(wrapped.toString), // TODO (KR) :
        wrapped :: Nil,
      )

  /**
    * Used when the user of the program provides invalid input.
    */
  final class UserError private (userMessage: String, internalMessage: String, causes: List[Throwable])
      extends HError.Single(
        HError.UserMessage.Const(userMessage),
        internalMessage,
        causes,
      )
  object UserError extends ErrorBuilder3[UserError](new UserError(_, _, _))

  /**
    * Used when there is an error with the system.
    * - IO
    * - Database
    * - Environment
    */
  final class SystemFailure private (internalMessage: String, causes: List[Throwable])
      extends HError.Single(
        HError.UserMessage.hidden,
        internalMessage,
        causes,
      )
  object SystemFailure extends ErrorBuilder2[SystemFailure](new SystemFailure(_, _))

  /**
    * Used when you want to start the error message with "WTF, why is this happening"
    * - Map#get(_).thisShouldAlwaysBeThere
    * - List#toNel.thisShouldAlwaysBeNonEmpty
    */
  final class InternalDefect private (internalMessage: String, causes: List[Throwable])
      extends HError.Single(
        HError.UserMessage.hidden,
        s"Internal Defect - unexpected failure:\n$internalMessage",
        causes,
      )
  object InternalDefect extends ErrorBuilder2[InternalDefect](new InternalDefect(_, _))

  /**
    * Analogous to the scala predef '???', except instead of throwing, it gives you a concrete error type.
    */
  final class ??? private (functionality: String)
      extends HError.Single(
        HError.UserMessage.Const(s"This feature is not yet supported: '$functionality'"),
        s"Encountered an unimplemented block of code: '$functionality'",
        Nil,
      )
  object ??? {
    def apply(functionality: String): ??? = new ???(functionality)
  }

  // =====| Helpers |=====

  private def throwableMessage(throwable: Throwable, stackTrace: Boolean): String = {
    val parts: (String, List[List[(String, String)]]) =
      throwable match {
        case hError: HError =>
          (
            s"HError[${hError.getClass.getSimpleName}]",
            List(
              hError.userMessage match {
                case UserMessage.Const(msg) if msg == hError.internalMessage => ("Message", msg) :: Nil
                case UserMessage.Const(msg)                                  => ("User Message", msg) :: ("Internal Message", hError.internalMessage) :: Nil
                case _                                                       => ("Internal Message", hError.internalMessage) :: Nil
              },
              hError.causes.zipWithIndex.map { (c, i) => (s"Cause[$i]", throwableMessage(c, stackTrace)) },
              Option.when(stackTrace)(("Stack Trace", formatExceptionTrace(hError.getStackTrace))).toList,
            ),
          )
        case throwable =>
          (
            throwable.getClass.getSimpleName,
            List(
              Option(throwable.getMessage).map(("Message", _)).toList,
              Option(throwable.getCause).map { c => ("Cause", throwableMessage(c, stackTrace)) }.toList,
              Option.when(stackTrace)(("Stack Trace", formatExceptionTrace(throwable.getStackTrace))).toList,
            ),
          )
      }

    parts._2.flatten
      .map { (label, message) => message.split("\n").mkString(s"\n  $label:\n      > ", "\n        ", "") }
      .mkString(s"--- ${parts._1} ---", "", "")
  }

  private def formatExceptionTrace(trace: Array[StackTraceElement]): String = trace.mkString("\n")

  private def unwrapThrowable(throwable: Throwable): List[Throwable] =
    throwable match {
      case HError.Multiple(children) => children.toList.flatMap(unwrapThrowable)
      case HError.Wrapped(throwable) => throwable :: Nil
      case _                         => throwable :: Nil
    }

  // =====| Builders |=====

  abstract class ErrorBuilder2[+E](build: (String, List[Throwable]) => E) {
    final def apply(message: String, causes: Throwable*): E = build(message, causes.toList.flatMap(HError.unwrapThrowable))
    final def apply(message: String, causes: Iterable[Throwable]): E = build(message, causes.toList.flatMap(HError.unwrapThrowable))
  }

  abstract class ErrorBuilder3[+E](build: (String, String, List[Throwable]) => E) extends ErrorBuilder2[E]((msg, causes) => build(msg, msg, causes)) {
    final def apply(userMessage: String, internalMessage: String, causes: Throwable*): E = build(userMessage, internalMessage, causes.toList.flatMap(HError.unwrapThrowable))
    final def apply(userMessage: String, internalMessage: String, causes: Iterable[Throwable]): E = build(userMessage, internalMessage, causes.toList.flatMap(HError.unwrapThrowable))
  }

}
