package harness.sql.typeclass

import harness.sql.*
import shapeless3.deriving.*

trait TableCols[T[_[_]]] {
  def columns(t: T[AppliedCol]): List[ColRef]
}

object TableCols {

  given [T]: TableCols[K11.Id[T]] =
    (t: AppliedCol[T]) => t.ref :: Nil

  given tableColsGen[H[_[_]]](using inst: => K11.ProductInstances[TableCols, H]): TableCols[H] =
    (t: H[AppliedCol]) =>
      inst.foldRight(t)(List.empty[ColRef]) {
        [a[_[_]]] => (ft: TableCols[a], c: a[AppliedCol], acc: List[ColRef]) => ft.columns(c) ::: acc
      }

  inline def derived[F[_[_]]](using gen: K11.ProductGeneric[F]): TableCols[F] = tableColsGen

}
