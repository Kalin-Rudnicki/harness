package harness.sql

import harness.sql.typeclass.*
import shapeless3.deriving.*

final case class TableSchema[T[_[_]] <: Table](
    tableName: String,
    colInfo: T[Col],
    functorK: FunctorK[T],
    tableCols: TableCols[T],
    rowCodec: RowCodec[T[Id]],
    createQuery: String,
    insertQuery: String,
)
object TableSchema {

  inline def derived[T[_[_]] <: Table](tableName: String)(colInfo: T[Col])(using gen: K11.ProductGeneric[T]): TableSchema[T] = {
    val functorK: FunctorK[T] = FunctorK.derived[T]
    val rowCodec: RowCodec[T[Id]] =
      RowCodec(
        RowEncoder.forTable[T](functorK.mapK(colInfo) { [a] => (c: Col[a]) => c.colCodec.encoder }),
        RowDecoder.forTable[T](functorK.mapK(colInfo) { [a] => (c: Col[a]) => c.colCodec.decoder }),
      )
    val cols: IArray[Col[Any]] = colArray(colInfo)
    TableSchema[T](
      tableName = tableName,
      colInfo = colInfo,
      functorK = functorK,
      tableCols = TableCols.derived[T],
      rowCodec = rowCodec,
      createQuery = (
        cols
          .map { c =>
            s"${c.colName} ${c.colType}${if (c.nullable) "" else " NOT NULL"}"
          }
          .mkString(s"CREATE TABLE $tableName (", ", ", ")"),
      ),
      insertQuery = s"INSERT INTO $tableName (${cols.map(_.colName).mkString(", ")}) VALUES (${0.until(rowCodec.encoder.width).map(_ => "?").mkString(", ")})",
    )
  }

  private def colArray[T[_[_]] <: Table](colInfo: T[Col])(using gen: => K11.ProductGeneric[T]): IArray[Col[Any]] =
    gen.toRepr(colInfo).toIArray.map(_.asInstanceOf[Col[Any]])

}
