package harness.sql.typeclass

import harness.core.Zip
import harness.sql.*
import java.sql.PreparedStatement
import scala.annotation.tailrec
import shapeless3.deriving.*

trait RowEncoder[T] { self =>

  lazy val width: Int
  def encodeRow(t: T, o: Int, arr: Array[Object]): Unit

  final def cmap[T2](f: T2 => T): RowEncoder[T2] =
    new RowEncoder[T2] {
      override lazy val width: Int = self.width
      override def encodeRow(t: T2, o: Int, arr: Array[Object]): Unit = self.encodeRow(f(t), o, arr)
    }

  final def ~[T2](other: RowEncoder[T2])(implicit zip: Zip[T, T2]): RowEncoder[zip.Out] =
    new RowEncoder[zip.Out] {
      override lazy val width: Int = self.width + other.width
      override def encodeRow(t: zip.Out, o: Int, arr: Array[Object]): Unit = {
        val (a, b) = zip.unzip(t)
        self.encodeRow(a, o, arr)
        other.encodeRow(b, o + self.width, arr)
      }
    }

}
object RowEncoder {

  def fromColEncoder[T](ce: ColEncoder[T]): RowEncoder[T] =
    new RowEncoder[T] {
      override lazy val width: Int = 1
      override def encodeRow(t: T, o: Int, arr: Array[Object]): Unit = arr(o) = ce.encodeColumn(t)
    }

  def forTable[T[_[_]] <: Table](encoders: T[ColEncoder])(using inst: => K11.ProductGeneric[T]): RowEncoder[T[Id]] =
    new RowEncoder[T[Id]] {
      lazy val encodersTuple: Tuple = inst.toRepr(encoders)
      override lazy val width: Int = encodersTuple.size
      override def encodeRow(t: T[Id], o: Int, arr: Array[Object]): Unit = {
        val rowTuple: Tuple = inst.toRepr(t)
        rowTuple.zip(encodersTuple).toArray.asInstanceOf[Array[(Any, ColEncoder[Any])]].zipWithIndex.foreach { case ((t, e), i) =>
          arr(o + i) = e.encodeColumn(t)
        }
      }
    }

}
