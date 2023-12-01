package harness.sql.autoSchema

import harness.sql.*

final class Tables private (private[autoSchema] val tableSchemas: List[TableSchema.AnySchema])
object Tables {
  def apply(tableSchemas: TableSchema.AnySchema*): Tables = new Tables(tableSchemas.toList)
  def fromCompanions(companions: Table.Companion.AnyCompanion*): Tables = new Tables(companions.toList.map(_.tableSchema))

}
