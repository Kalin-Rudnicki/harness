package harness.sql.query

import cats.syntax.option.*
import harness.sql.*
import harness.sql.typeclass.*

final case class QuerySet private[sql] (
    private[sql] val fragment: Fragment,
) { self =>

  def ~(other: QuerySet): QuerySet =
    QuerySet(
      fr"$self, $other",
    )

}

trait QuerySetOps[A, B] {
  def build(a: A, B: B): QuerySet
}
object QuerySetOps {

  implicit def col_id[A]: QuerySetOps[AppliedCol[A], QueryInputVar[A]] = { (appliedCol, inputVar) =>
    val inputVarFragment = Fragment(
      "?",
      QueryInputMapper.single[A](_(inputVar.idx).asInstanceOf[A], appliedCol.col.codec.encoder),
    )

    QuerySet(fr"${appliedCol.col.colName} = $inputVarFragment")
  }

  implicit def oCol_id[A]: QuerySetOps[AppliedCol[Option[A]], QueryInputVar[A]] = { (appliedCol, inputVar) =>
    val inputVarFragment = Fragment(
      "?",
      QueryInputMapper.single[Option[A]](_(inputVar.idx).asInstanceOf[A].some, appliedCol.col.codec.encoder),
    )

    QuerySet(fr"${appliedCol.col.colName} = $inputVarFragment")
  }

  implicit def col_const[A]: QuerySetOps[AppliedCol[A], Constant[A]] = { (appliedCol, const) =>
    val constFragment = Fragment(
      "?",
      QueryInputMapper.materialize(const, appliedCol.col.codec.encoder),
    )

    QuerySet(fr"${appliedCol.col.colName} = $constFragment")
  }

  implicit def oCol_const[A]: QuerySetOps[AppliedCol[Option[A]], Constant[A]] = { (appliedCol, const) =>
    val constFragment = Fragment(
      "?",
      QueryInputMapper.materialize(const.map(_.some), appliedCol.col.codec.encoder),
    )

    QuerySet(fr"${appliedCol.col.colName} = $constFragment")
  }

}

extension [A](a: A) {
  def :=[B](b: B)(implicit qso: QuerySetOps[A, B]): QuerySet = qso.build(a, b)
}
