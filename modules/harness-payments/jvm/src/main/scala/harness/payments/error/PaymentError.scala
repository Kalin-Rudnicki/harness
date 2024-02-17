package harness.payments.error

sealed trait PaymentError extends Throwable
object PaymentError {
  final case class FailedToCreateParams(paramType: String, cause: Throwable) extends PaymentError
  final case class FailedToMakeCall(call: String, cause: Throwable) extends PaymentError
  final case class InternalDefect(message: String) extends PaymentError
}
