package harness.zio

import harness.zio.ZIOJsonInstances.throwableJsonCodec
import zio.*
import zio.json.*

final case class ErrorLogger[-E](
    convert: E => (Logger.LogLevel, String),
) {

  def log(e: E, context: (String, Any)*): URIO[Logger, Unit] = {
    val (level, message) = convert(e)
    Logger.log(level, message, context*)
  }

  def logCause(e: Cause[E], causeLevel: Logger.LogLevel, stackTraceLevel: Option[Logger.LogLevel], context: (String, Any)*): URIO[Logger, Unit] = {
    inline def logTrace(trace: StackTrace): URIO[Logger, Unit] =
      ZIO.foreachDiscard(stackTraceLevel)(Logger.log(_, trace.prettyPrint))

    e match {
      case Cause.Empty => ZIO.unit
      case Cause.Fail(value, trace) =>
        log(value, context*) *>
          logTrace(trace)
      case Cause.Die(value, trace) =>
        Logger.log(causeLevel, s"ZIO Died (${value.getClass.getName}): ${Option(value.getMessage).getOrElse(value.toString)}", context*) *>
          logTrace(trace)
      case Cause.Interrupt(fiberId, trace) =>
        Logger.log(causeLevel, s"Interrupted: $fiberId", context*) *>
          logTrace(trace)
      case Cause.Stackless(cause, stackless) =>
        logCause(cause, causeLevel, Option.when(!stackless)(stackTraceLevel).flatten, context*)
      case Cause.Then(left, right) =>
        logCause(left, causeLevel, stackTraceLevel, context*) *>
          logCause(right, causeLevel, stackTraceLevel, context*)
      case Cause.Both(left, right) =>
        logCause(left, causeLevel, stackTraceLevel, context*) *>
          logCause(right, causeLevel, stackTraceLevel, context*)
    }
  }

  // If there are any Cause.Fail, don't log other stuff
  def logCauseSimple(e: Cause[E], causeLevel: Logger.LogLevel, stackTraceLevel: Option[Logger.LogLevel], context: (String, Any)*): URIO[Logger, Unit] =
    e.causeFailuresOpt match {
      case Some(causeFailures) => ZIO.foreachDiscard(causeFailures.toList)(logCause(_, causeLevel, stackTraceLevel, context*))
      case None                => logCause(e, causeLevel, stackTraceLevel, context*)
    }

  def withPrefix(prefix: String): ErrorLogger[E] =
    ErrorLogger[E] { e =>
      val (level, str) = convert(e)
      (level, s"$prefix$str")
    }

}
object ErrorLogger {

  def apply[E](implicit errorLogger: ErrorLogger[E]): ErrorLogger[E] = errorLogger

  def make[E](convert: E => (Logger.LogLevel, String)): ErrorLogger[E] = ErrorLogger(convert)

  def withToString[E]: ErrorLogger.Builder[E] = new Builder[E](_.toString)
  def withGetMessage[E <: Throwable]: ErrorLogger.Builder[E] = new Builder[E](e => Option(e.getMessage).getOrElse(e.toString))
  def withShow[E](f: E => String): ErrorLogger.Builder[E] = new Builder[E](f)
  def withJsonShow[E: JsonEncoder]: ErrorLogger.Builder[E] = new Builder[E](_.toJson)
  def withJsonPrettyShow[E: JsonEncoder]: ErrorLogger.Builder[E] = new Builder[E](_.toJsonPretty)

  final class Builder[E] private[ErrorLogger] (show: E => String) {

    def atLevel: WithLogLevel[ErrorLogger[E]] = WithLogLevel.make { level => ErrorLogger[E](e => (level, show(e))) }

    def withLevel(f: E => Logger.LogLevel): ErrorLogger[E] = ErrorLogger[E](e => (f(e), show(e)))

  }

  implicit val nothingErrorLogger: ErrorLogger[Nothing] =
    ErrorLogger.withToString[Nothing].atLevel.always

  val throwablePrettyErrorLogger: ErrorLogger[Throwable] =
    ErrorLogger
      .withShow[Throwable] {
        case jsonShowable: JsonShowable[?] => jsonShowable.showJsonPretty
        case t                             => t.toJsonPretty
      }
      .atLevel
      .error

}
