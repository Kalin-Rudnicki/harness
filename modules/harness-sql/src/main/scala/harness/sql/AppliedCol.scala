package harness.sql

import harness.deriving.K11.~>

final case class AppliedCol[T](tableVarName: String, col: Col[T]) {
  def ref: ColRef = ColRef(tableVarName, col.colName)
  def show: String = s"$tableVarName.${col.colName}"
}
object AppliedCol {

  final case class Opt[T](wrapped: AppliedCol[T])

  def withVarName(tableVarName: String): Col ~> AppliedCol =
    [a] => (fa: Col[a]) => AppliedCol(tableVarName, fa)

  def optional: AppliedCol ~> AppliedCol.Opt =
    [a] => (fa: AppliedCol[a]) => Opt(fa)

}
