package harness.serviceTracer

final case class TraceClosure(
    serviceName: String,
    serviceImpl: String,
    function: String,
) {
  override def toString: String = s"$serviceName[$serviceImpl].$function"
}
