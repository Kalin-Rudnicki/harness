package harness.sql.typeclass

import harness.sql.*
import shapeless3.deriving.*

trait TableToList[T[_[_]]] {
  def toList[F[_]](t: T[F]): List[F[Any]]
}

object TableToList {

  given [T]: TableToList[K11.Id[T]] =
    new TableToList[K11.Id[T]] {
      override def toList[F[_]](t: K11.Id[T][F]): List[F[Any]] = t.asInstanceOf[F[Any]] :: Nil
    }

  given TableToListGen[H[_[_]]](using inst: => K11.ProductInstances[TableToList, H]): TableToList[H] =
    new TableToList[H] {
      override def toList[F[_]](t: H[F]): List[F[Any]] =
        inst.foldRight(t)(List.empty[F[Any]]) {
          [a[_[_]]] => (ft: TableToList[a], c: a[F], acc: List[F[Any]]) => ft.toList(c) ::: acc
        }
    }

  inline def derived[F[_[_]]](using gen: K11.ProductGeneric[F]): TableToList[F] = TableToListGen

}
