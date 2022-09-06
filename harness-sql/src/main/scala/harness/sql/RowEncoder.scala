package harness.sql

trait RowEncoder[T] { self =>

  def encodeRow(t: T): List[ColT]

  final def cmap[T2](f: T2 => T): RowEncoder[T2] =
    t => self.encodeRow(f(t))

}
object RowEncoder {

  def fromColEncoder[T](ce: ColEncoder[T]): RowEncoder[T] =
    ce.encodeColumn(_) :: Nil

}
