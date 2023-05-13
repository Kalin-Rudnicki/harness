package harness.web

sealed abstract class HttpMethod(final val method: String)
object HttpMethod {

  def apply(method: String): HttpMethod = lookupMap.getOrElse(method, NonStandard(method))

  // =====|  |=====

  case object GET extends HttpMethod("GET")
  case object POST extends HttpMethod("POST")
  case object PUT extends HttpMethod("PUT")
  case object DELETE extends HttpMethod("DELETE")
  case object HEAD extends HttpMethod("HEAD")
  case object OPTIONS extends HttpMethod("OPTIONS")
  case object CONNECT extends HttpMethod("CONNECT")
  case object TRACE extends HttpMethod("TRACE")
  case object PATCH extends HttpMethod("PATCH")

  // =====|  |=====

  final case class NonStandard private[HttpMethod] (_method: String) extends HttpMethod(_method)

  private val lookupMap: Map[String, HttpMethod] =
    List[HttpMethod](
      HttpMethod.GET,
      HttpMethod.POST,
      HttpMethod.PUT,
      HttpMethod.DELETE,
      HttpMethod.HEAD,
      HttpMethod.OPTIONS,
      HttpMethod.CONNECT,
      HttpMethod.TRACE,
      HttpMethod.PATCH,
    ).map(m => m.method -> m).toMap

}
