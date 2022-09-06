package harness.sql

final case class ColRef(tableVarName: String, colName: String) {
  override def toString: String = s"$tableVarName.$colName"
}
