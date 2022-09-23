package harness.web

final case class HttpMethod(method: String)
object HttpMethod {
  val GET: HttpMethod = HttpMethod("GET")
  val POST: HttpMethod = HttpMethod("POST")
  val PUT: HttpMethod = HttpMethod("PUT")
  val DELETE: HttpMethod = HttpMethod("DELETE")
}
