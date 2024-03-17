package harness.payments.error

import harness.core.*

sealed trait PaymentError extends Throwable {

  override final def getMessage: String = this match {
    case PaymentError.FailedToCreateParams(paramType, cause) =>
      s"Failed to create payment params '$paramType': ${cause.safeGetMessage}"
    case PaymentError.FailedToMakeCall(call, cause) =>
      s"Failed to make payment call '$call': ${cause.safeGetMessage}"
    case PaymentError.InternalDefect(message) =>
      s"Payment encountered internal defect: $message"
  }

}
object PaymentError {
  final case class FailedToCreateParams(paramType: String, cause: Throwable) extends PaymentError
  final case class FailedToMakeCall(call: String, cause: Throwable) extends PaymentError
  final case class InternalDefect(message: String) extends PaymentError
}
