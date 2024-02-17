package harness.sql

import shapeless3.deriving.*

final case class AppliedCol[T](tableVarName: String, col: Col[T]) {

  def ref: ColRef = ColRef(tableVarName, col.colName, col.getType)

  override def toString: String = s"$tableVarName.$col"

}
object AppliedCol {

  final case class Opt[T](wrapped: AppliedCol[T])

  def withVarName(tableVarName: String): Col ~> AppliedCol =
    [a] => (fa: Col[a]) => AppliedCol(tableVarName, fa)

  def optional: AppliedCol ~> AppliedCol.Opt =
    [a] => (fa: AppliedCol[a]) => Opt(fa)

}
