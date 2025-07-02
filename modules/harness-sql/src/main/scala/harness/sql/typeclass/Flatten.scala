package harness.sql.typeclass

import harness.deriving.*
import harness.deriving.K11.*
import harness.sql.{Table, TableSchema}
import zio.Chunk

trait Flatten[T[_[_]] <: Table] {
  def apply[A[_]](t: T[A]): Chunk[A[Any]]
}
object Flatten {

  @scala.annotation.nowarn
  inline def derive[T[_[_]] <: Table](implicit gen: K11.ProductGeneric[T]): Flatten[T] =
    new Flatten[T] {
      override def apply[A[_]](t: T[A]): Chunk[A[Any]] = Chunk.fromArray { gen.toRepr(t).toArray.map(_.asInstanceOf[A[Any]]) }
    }

  implicit def fromTableSchema[T[_[_]] <: Table](implicit schema: TableSchema[T]): Flatten[T] = schema.flatten

}
