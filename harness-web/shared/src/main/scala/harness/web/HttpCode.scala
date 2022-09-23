package harness.web

final case class HttpCode(code: Int)
object HttpCode {
  val `200`: HttpCode = HttpCode(200)
  val `301`: HttpCode = HttpCode(301)
  val `400`: HttpCode = HttpCode(400)
  val `404`: HttpCode = HttpCode(404)
  val `500`: HttpCode = HttpCode(500)
}
