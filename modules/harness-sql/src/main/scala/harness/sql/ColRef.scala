package harness.sql

final case class ColRef(tableVarName: String, colName: String) {
  def show: String = s"$tableVarName.$colName"
}
