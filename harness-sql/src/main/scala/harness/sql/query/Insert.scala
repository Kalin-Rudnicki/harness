package harness.sql.query

import harness.sql.*
import harness.sql.typeclass.*
import shapeless3.deriving.Id

object Insert {

  def into[T[_[_]] <: Table](implicit ti: TableSchema[T]): Query[T[Id]] =
    Query(ti.insertQuery, ti.rowCodec.encoder)

  final class Query[I] private[Insert] (
      private[query] val query: String,
      private[query] val encoder: RowEncoder[I],
  )

}
