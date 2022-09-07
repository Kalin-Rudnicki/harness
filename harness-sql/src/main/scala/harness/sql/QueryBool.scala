package harness.sql

final case class QueryBool private[sql] (
    wrapped: String,
    unaryNeedsParens: Boolean,
    binaryNeedsParens: Boolean,
    queryInputMapper: QueryInputMapper,
) {

  def unaryString: String = if (unaryNeedsParens) s"($wrapped)" else wrapped
  def binaryString: String = if (binaryNeedsParens) s"($wrapped)" else wrapped

  def unary_! : QueryBool = QueryBool(s"!$unaryString", false, false, queryInputMapper)
  def &&(that: QueryBool): QueryBool =
    QueryBool(
      s"${this.binaryString} AND ${that.binaryString}",
      true,
      true,
      this.queryInputMapper + that.queryInputMapper,
    )
  def ||(that: QueryBool): QueryBool =
    QueryBool(
      s"${this.binaryString} OR ${that.binaryString}",
      true,
      true,
      this.queryInputMapper + that.queryInputMapper,
    )

}
