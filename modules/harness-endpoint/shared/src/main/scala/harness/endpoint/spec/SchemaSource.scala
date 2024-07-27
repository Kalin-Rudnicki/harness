package harness.endpoint.spec

import harness.schema.*
import zio.json.*

@jsonDiscriminator("type")
sealed abstract class SchemaSource(final val show: String) {
  override def toString: String = show
}
object SchemaSource {

  final case class Query(key: String) extends SchemaSource(s"query param '$key'")
  final case class Header(key: String) extends SchemaSource(s"header '$key'")
  final case class Cookie(key: String) extends SchemaSource(s"cookie '$key'")
  final case class HeaderOrCookie(key: String) extends SchemaSource(s"header/cookie '$key'")
  case object Body extends SchemaSource("body")

  implicit val schema: JsonSchema[SchemaSource] = JsonSchema.derived

}
