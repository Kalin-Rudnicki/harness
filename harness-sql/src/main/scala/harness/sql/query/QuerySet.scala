package harness.sql.query

import cats.syntax.option.*
import harness.sql.*

final case class QuerySet private[sql] (
    private[sql] val wrapped: String,
    private[sql] val queryInputMapper: QueryInputMapper,
) { self =>

  def ~(other: QuerySet): QuerySet =
    QuerySet(
      s"${self.wrapped}, ${other.wrapped}",
      self.queryInputMapper + other.queryInputMapper,
    )

}

trait QuerySetOps[A, B] {
  def build(a: A, B: B): QuerySet
}
object QuerySetOps {

  implicit def col_id[A]: QuerySetOps[AppliedCol[A], QueryInput[A]] =
    (a, b) =>
      QuerySet(
        s"${a.ref.colName} = ?",
        QueryInputMapper(_ => 1, (in, out, off) => out(off) = a.col.colCodec.encoder.encodeColumn(in(b.idx).asInstanceOf)),
      )
  implicit def oCol_id[A]: QuerySetOps[AppliedCol[Option[A]], QueryInput[A]] =
    (a, b) =>
      QuerySet(
        s"${a.ref.colName} = ?",
        QueryInputMapper(_ => 1, (in, out, off) => out(off) = a.col.colCodec.encoder.encodeColumn(in(b.idx).some.asInstanceOf)),
      )

}

extension [A](a: A) {
  def :=[B](b: B)(implicit qso: QuerySetOps[A, B]): QuerySet = qso.build(a, b)
}