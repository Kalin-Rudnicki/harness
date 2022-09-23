package harness.sql.autoSchema

import harness.sql.*

final class Tables private (private[autoSchema] val tableSchemas: List[TableSchema[_ <: ([_[_]] =>> Table)]])
object Tables {
  def apply(tableSchemas: TableSchema[_ <: ([_[_]] =>> Table)]*): Tables = new Tables(tableSchemas.toList)
}
