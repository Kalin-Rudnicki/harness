package harness.sql

import harness.sql.typeclass.*
import shapeless3.deriving.*

final case class TableSchema[T[_[_]] <: Table](
    tableSchema: String,
    tableName: String,
    colInfo: T[Col],
    functorK: FunctorK[T],
    tableCols: TableCols[T],
    rowCodec: RowCodec[T[Id]],
    colList: List[Col[Any]],
    insertQuery: String,
) {
  lazy val referenceName: String = s"$tableSchema.$tableName"
}
object TableSchema {

  inline def derived[T[_[_]] <: Table](tableSchema: String, tableName: String)(colInfo: T[Col])(using gen: K11.ProductGeneric[T]): TableSchema[T] = {
    val functorK: FunctorK[T] = FunctorK.derived[T]
    val rowCodec: RowCodec[T[Id]] =
      RowCodec(
        RowEncoder.forTable[T](functorK.mapK(colInfo) { [a] => (c: Col[a]) => c.colCodec.encoder }),
        RowDecoder.forTable[T](functorK.mapK(colInfo) { [a] => (c: Col[a]) => c.colCodec.decoder }),
      )
    val cols: IArray[Col[Any]] = colArray(colInfo)
    TableSchema[T](
      tableSchema = tableSchema,
      tableName = tableName,
      colInfo = colInfo,
      functorK = functorK,
      tableCols = TableCols.derived[T],
      rowCodec = rowCodec,
      colList = cols.toList,
      insertQuery = {
        val colsString = cols.map(_.colName).mkString(", ")
        val qMarks = cols.map(_.?).mkString(", ")
        s"INSERT INTO $tableSchema.$tableName ($colsString) VALUES ($qMarks)" // RETURNING $colsString"
      },
    )
  }

  inline def derived[T[_[_]] <: Table](tableName: String)(colInfo: T[Col])(using gen: K11.ProductGeneric[T]): TableSchema[T] =
    TableSchema.derived[T]("public", tableName)(colInfo)

  private def colArray[T[_[_]] <: Table](colInfo: T[Col])(using gen: => K11.ProductGeneric[T]): IArray[Col[Any]] =
    gen.toRepr(colInfo).toIArray.map(_.asInstanceOf[Col[Any]])

}
