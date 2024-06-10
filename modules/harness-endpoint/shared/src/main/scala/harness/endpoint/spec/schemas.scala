package harness.endpoint.spec

import harness.core.Enum
import harness.schema.Schema

sealed trait PathSchema {

  final def showBasic: String = this match
    case PathSchema.Const(p)       => p
    case PathSchema.Param(name, _) => s"{{$name}}"
    case PathSchema.Rest(name)     => s"{{$name}}*"
    case PathSchema.RestNel(name)  => s"{{$name}}**"

}
object PathSchema {
  final case class Const(p: String) extends PathSchema
  final case class Param(name: String, schema: Schema[?]) extends PathSchema
  final case class Rest(name: String) extends PathSchema
  final case class RestNel(name: String) extends PathSchema
}

enum SchemaRequirement(final val suffix: String) extends Enum[SchemaRequirement] {
  case Required extends SchemaRequirement("")
  case Optional extends SchemaRequirement("?")
  case Many extends SchemaRequirement("*")
  case ManyNonEmpty extends SchemaRequirement("**")
}
object SchemaRequirement extends Enum.Companion[SchemaRequirement]

sealed abstract class NonPathSchema(final val source: SchemaSource) {
  val requirement: SchemaRequirement
}

final case class QuerySchema(requirement: SchemaRequirement, key: String, schema: Schema[?]) extends NonPathSchema(SchemaSource.Query(key)) {
  def showBasic: String = s"{{$key}}${requirement.suffix}"
}

sealed abstract class GenericHeaderSchema(source: SchemaSource) extends NonPathSchema(source) {
  val requirement: SchemaRequirement
  val key: String
  final def showBasic: String = s"{{$key}}${requirement.suffix}"
}

final case class HeaderSchema(requirement: SchemaRequirement, key: String, schema: Schema[?]) extends GenericHeaderSchema(SchemaSource.Header(key))

final case class CookieSchema(requirement: SchemaRequirement, key: String, schema: Schema[?]) extends GenericHeaderSchema(SchemaSource.Cookie(key))

final case class HeaderOrCookieSchema(requirement: SchemaRequirement, key: String, schema: Schema[?]) extends GenericHeaderSchema(SchemaSource.HeaderOrCookie(key))
