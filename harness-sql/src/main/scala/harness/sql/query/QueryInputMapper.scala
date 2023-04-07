package harness.sql.query

final case class QueryInputMapper(
    private[sql] val width: IArray[Object] => Int,
    private[sql] val prepare: (IArray[Object], Array[Object], Int) => Unit,
) { self =>

  def +(other: QueryInputMapper): QueryInputMapper =
    QueryInputMapper(
      in => self.width(in) + other.width(in),
      { (in, out, off) =>
        self.prepare(in, out, off)
        other.prepare(in, out, off + self.width(in))
      },
    )

}
object QueryInputMapper {
  val empty: QueryInputMapper = QueryInputMapper(_ => 0, (_, _, _) => ())
  val id: QueryInputMapper = QueryInputMapper(_.length, (in, out, off) => in.copyToArray(out, off, in.length))

  def single[A](getObj: IArray[Object] => Object)(objSize: A => Int): QueryInputMapper =
    QueryInputMapper(in => objSize(getObj(in).asInstanceOf[A]), (in, out, off) => out(off) = getObj(in))
  def single(getObj: IArray[Object] => Object): QueryInputMapper =
    QueryInputMapper(_ => 1, (in, out, off) => out(off) = getObj(in))

}
