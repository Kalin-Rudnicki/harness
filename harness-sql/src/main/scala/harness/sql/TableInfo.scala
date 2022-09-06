package harness.sql

import shapeless3.deriving.*

final case class TableInfo[T[_[_]] <: Table](
    final val tableName: String,
    final val colInfo: T[Col],
    final val functorK: FunctorK[T],
    final val tableCols: TableInfo.TableCols[T],
)
object TableInfo {

  inline def derived[T[_[_]] <: Table](tableName: String)(colInfo: T[Col])(using gen: K11.ProductGeneric[T]): TableInfo[T] =
    new TableInfo[T](
      tableName = tableName,
      colInfo = colInfo,
      functorK = FunctorK.derived[T],
      tableCols = TableCols.derived[T],
    )

  trait TableCols[T[_[_]]] {
    def columns(t: T[AppliedCol]): List[ColRef]
  }
  object TableCols {

    given [T]: TableCols[K11.Id[T]] =
      (t: AppliedCol[T]) => t.ref :: Nil

    given tableColsGen[H[_[_]]](using inst: => K11.ProductInstances[TableCols, H]): TableCols[H] =
      (t: H[AppliedCol]) => inst.foldRight(t)(List.empty[ColRef])([a[_[_]]] => (ft: TableCols[a], c: a[AppliedCol], acc: List[ColRef]) => ft.columns(c) ::: acc)

    inline def derived[F[_[_]]](using gen: K11.ProductGeneric[F]): TableCols[F] = tableColsGen

  }

  trait ColInfo[T[_[_]]] {
    def gen: T[Col]
  }
  object ColInfo {

    given colInfoGen[H[_[_]]](using
        inst: => K11.ProductInstances[TableCols, H],
        labels: => Labelling[H[cats.Id]],
        annotations: Annotations[Col.Name, H[cats.Id]],
    ): ColInfo[H] =
      new ColInfo[H] {
        override def gen: H[Col] = {
          // inst.construct { [t[_[_]]] => (f: TableCols[t]) => (??? : t[Col]) }

          // names: List[String] = annotation || toSnake(colName)
          // gens: List[Col.GenCol[_]] = summon (somehow?)

          // inst.fromRepr(names.zip(gens).map { (n, g) => g.make(n) }.toTuple) (?)

          ??? // TODO (KR) :
        }
      }

  }

}
