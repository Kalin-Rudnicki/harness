package harness.sql

import harness.sql.typeclass.*
import shapeless3.deriving.*

final case class TableInfo[T[_[_]] <: Table](
    tableName: String,
    colInfo: T[Col],
    functorK: FunctorK[T],
    tableCols: TableCols[T],
    rowCodec: RowCodec[T[cats.Id]],
)
object TableInfo {

  inline def derived[T[_[_]] <: Table](tableName: String)(colInfo: T[Col])(using gen: K11.ProductGeneric[T]): TableInfo[T] = {
    val functorK: FunctorK[T] = FunctorK.derived[T]
    new TableInfo[T](
      tableName = tableName,
      colInfo = colInfo,
      functorK = functorK,
      tableCols = TableCols.derived[T],
      rowCodec = RowCodec(
        RowEncoder.forTable[T](functorK.mapK(colInfo) { [a] => (c: Col[a]) => RowEncoder.fromColEncoder(c.colCodec.encoder) }),
        RowDecoder.forTable[T](functorK.mapK(colInfo) { [a] => (c: Col[a]) => RowDecoder.fromColDecoder(c.colCodec.decoder) }),
      ),
    )
  }

}
