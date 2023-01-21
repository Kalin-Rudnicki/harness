package harness.sql

final case class ColRef(tableVarName: String, colName: String, getType: Option[String]) {
  override def toString: String = getType.fold(s"$tableVarName.$colName")(t => s"$tableVarName.$colName :: $t")
}
