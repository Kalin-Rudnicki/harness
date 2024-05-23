package harness.sql.query

final case class QueryInputVar[T] private[sql] (private[sql] val idx: Int)
