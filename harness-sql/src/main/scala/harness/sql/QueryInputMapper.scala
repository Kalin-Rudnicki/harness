package harness.sql

final case class QueryInputMapper(
    width: IArray[Any] => Int,
    prepare: (IArray[Any], Array[Object], Int) => Unit,
) { self =>
  
  
  def +(other: QueryInputMapper): QueryInputMapper =
    QueryInputMapper(
      in => self.width(in) + other.width(in),
      { (in, out, off) =>
        self.prepare(in, out, off)
        other.prepare(in, out, off + this.width(in))
      },
    )
  
}
object QueryInputMapper {
  val empty: QueryInputMapper = QueryInputMapper(_ => 0, (_, _, _) => ())
}