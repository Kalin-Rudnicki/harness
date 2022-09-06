package harness.sql

final case class QueryBool private[sql] (wrapped: String, unaryNeedsParens: Boolean, binaryNeedsParens: Boolean) {

  def unaryString: String = if (unaryNeedsParens) s"($wrapped)" else wrapped
  def binaryString: String = if (binaryNeedsParens) s"($wrapped)" else wrapped

  def unary_! : QueryBool = QueryBool(s"!$unaryString", false, false)
  def &&(that: QueryBool): QueryBool = QueryBool(s"${this.binaryString} AND ${that.binaryString}", true, true)
  def ||(that: QueryBool): QueryBool = QueryBool(s"${this.binaryString} OR ${that.binaryString}", true, true)

}
