package harness.sql

final case class ForeignKeyRef private (schemaName: String, tableName: String, colName: String)
object ForeignKeyRef {
  def apply(schemaName: String, tableName: String, colName: String): ForeignKeyRef = new ForeignKeyRef(schemaName, tableName, colName)
  def apply(tableName: String, colName: String): ForeignKeyRef = new ForeignKeyRef("public", tableName, colName)
}
