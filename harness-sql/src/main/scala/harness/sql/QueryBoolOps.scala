package harness.sql

given Conversion[AppliedCol[Boolean], QueryBool] = a => QueryBool(s"${a.tableVarName}.${a.col.colName}", false, false)
given Conversion[AppliedCol.Opt[Boolean], QueryBool] = a => QueryBool(s"${a.wrapped.tableVarName}.${a.wrapped.col.colName}", false, false)

trait QueryBoolOps[A, B] private {
  def build(a: A, b: B): (String, String)
  final def build(a: A, b: B, op: String): QueryBool = {
    val (as, bs) = build(a, b)
    QueryBool(s"$as $op $bs", true, false)
  }
}
object QueryBoolOps {
  implicit def col_id[A]: QueryBoolOps[AppliedCol[A], A] = (a, b) => (a.ref.toString, a.col.colCodec.encodeColumn(b))
  implicit def col_col[A]: QueryBoolOps[AppliedCol[A], AppliedCol[A]] = (a, b) => (a.ref.toString, b.ref.toString)
  implicit def col_optCol[A]: QueryBoolOps[AppliedCol[A], AppliedCol.Opt[A]] = (a, b) => (a.ref.toString, b.wrapped.ref.toString)
  implicit def optCol_id[A]: QueryBoolOps[AppliedCol.Opt[A], A] = (a, b) => (a.wrapped.ref.toString, a.wrapped.col.colCodec.encodeColumn(b))
  implicit def optCol_col[A]: QueryBoolOps[AppliedCol.Opt[A], AppliedCol[A]] = (a, b) => (a.wrapped.ref.toString, b.ref.toString)
  implicit def optCol_optCol[A]: QueryBoolOps[AppliedCol.Opt[A], AppliedCol.Opt[A]] = (a, b) => (a.wrapped.ref.toString, b.wrapped.ref.toString)
}

extension [A](a: A) {
  def ===[B](b: B)(implicit qbo: QueryBoolOps[A, B]): QueryBool = qbo.build(a, b, "=")
  def !==[B](b: B)(implicit qbo: QueryBoolOps[A, B]): QueryBool = qbo.build(a, b, "!=")
  // TODO (KR) : <, <=, >, >=, LIKE
}

extension [A](a: AppliedCol[Option[A]]) {
  def isNull: QueryBool = QueryBool(s"${a.ref} IS NULL", true, false)
  def isNotNull: QueryBool = QueryBool(s"${a.ref} IS NOT NULL", true, false)
}

extension [A](a: AppliedCol.Opt[A]) {
  def isNull: QueryBool = QueryBool(s"${a.wrapped.ref} IS NULL", true, false)
  def isNotNull: QueryBool = QueryBool(s"${a.wrapped.ref} IS NOT NULL", true, false)
}
