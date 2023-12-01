package harness.sql.autoSchema

import harness.sql.Col
import zio.json.*

sealed trait KeyType {

  final def sql: String =
    this match {
      case KeyType.NoKey              => ""
      case KeyType.PrimaryKey         => " PRIMARY KEY"
      case KeyType.ForeignKey(colRef) => s" REFERENCES ${colRef.schemaRef}.${colRef.tableName}(${colRef.colName})"
    }

}
object KeyType {

  case object NoKey extends KeyType
  case object PrimaryKey extends KeyType
  final case class ForeignKey(colRef: ColRef) extends KeyType

  def apply(keyType: Option[Col.KeyType]): KeyType =
    keyType match {
      case Some(Col.KeyType.PrimaryKey)                                 => KeyType.PrimaryKey
      case Some(Col.KeyType.ForeignKey(schemaName, tableName, colName)) => KeyType.ForeignKey(ColRef(SchemaRef(schemaName), tableName, colName))
      case None                                                         => KeyType.NoKey
    }

  implicit val jsonCodec: JsonCodec[KeyType] = DeriveJsonCodec.gen

}
