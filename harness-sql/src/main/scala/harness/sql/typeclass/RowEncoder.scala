package harness.sql.typeclass

import harness.core.Zip
import harness.sql.*
import shapeless3.deriving.*

trait RowEncoder[T] { self =>

  def encodeRow(t: T): List[ColT]

  final def cmap[T2](f: T2 => T): RowEncoder[T2] =
    t => self.encodeRow(f(t))

  final def ~[T2](other: RowEncoder[T2])(implicit zip: Zip[T, T2]): RowEncoder[zip.Out] = { t =>
    val (t1, t2) = zip.unzip(t)
    self.encodeRow(t1) ::: other.encodeRow(t2)
  }

}
object RowEncoder {

  def fromColEncoder[T](ce: ColEncoder[T]): RowEncoder[T] =
    ce.encodeColumn(_) :: Nil

  def forTable[T[_[_]] <: Table](encoders: T[RowEncoder])(using inst: => K11.ProductGeneric[T]): RowEncoder[T[cats.Id]] = { row =>
    val rowTuple: Tuple = inst.toRepr(row)
    val encodersTuple: Tuple = inst.toRepr(encoders)
    val zipped: List[(Any, RowEncoder[Any])] = rowTuple.zip(encodersTuple).toList.asInstanceOf[List[(Any, RowEncoder[Any])]]
    val encoded: List[ColT] = zipped.flatMap { (r, e) => e.encodeRow(r) }

    encoded
  }

}
