package harness.sql.query

import harness.deriving.*
import harness.sql.*

object Insert {

  def into[T[_[_]] <: Table](implicit ti: TableSchema[T]): Query[T[K11.Identity]] =
    Insert.Query(ti.insertFragment)

  final class Query[I] private[Insert] (
      private[query] val fragment: Fragment,
  )

}
