package harness.sql.autoSchema

import cats.syntax.either.*
import harness.sql.TableSchema
import zio.json.*

sealed abstract class SchemaRef(final val schemaName: String) {
  override final def toString: String = schemaName
}
object SchemaRef {
  case object Public extends SchemaRef("public")
  final case class Custom private[SchemaRef] (name: String) extends SchemaRef(name)
  object Custom {
    implicit val jsonCodec: JsonCodec[SchemaRef.Custom] =
      SchemaRef.jsonCodec.transformOrFail(
        {
          case custom @ Custom(_) => custom.asRight
          case Public             => "public is not allowed".asLeft
        },
        identity,
      )
  }

  def apply(name: String): SchemaRef =
    name match {
      case "public" => SchemaRef.Public
      case _        => SchemaRef.Custom(name)
    }

  implicit val jsonCodec: JsonCodec[SchemaRef] = JsonCodec.string.transform(SchemaRef(_), _.schemaName)

}

final case class TableRef(schemaRef: SchemaRef, tableName: String)
object TableRef {

  def apply(schema: TableSchema.AnySchema): TableRef =
    TableRef(SchemaRef(schema.tableSchema), schema.tableName)

  implicit val jsonCodec: JsonCodec[TableRef] = DeriveJsonCodec.gen

}

final case class ColRef(schemaRef: SchemaRef, tableName: String, colName: String) {
  def tableRef: TableRef = TableRef(schemaRef, tableName)
}
object ColRef {
  implicit val jsonCodec: JsonCodec[ColRef] = DeriveJsonCodec.gen
}
