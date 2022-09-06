package harness.sql

import shapeless3.deriving.*

final case class AppliedCol[T](tableVarName: String, col: Col[T]) {

  def ref: ColRef = ColRef(tableVarName, col.colName)

  final def optional: AppliedCol[Option[T]] = AppliedCol(tableVarName, col.optional)

  override def toString: String = s"$tableVarName.$col"

}
object AppliedCol {

  type Opt[A] = AppliedCol[Option[A]]

  def withVarName(tableVarName: String): Col ~> AppliedCol =
    [a] => (fa: Col[a]) => AppliedCol(tableVarName, fa)

}
