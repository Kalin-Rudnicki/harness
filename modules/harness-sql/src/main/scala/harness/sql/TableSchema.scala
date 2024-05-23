package harness.sql

import harness.deriving.*
import harness.sql.query.*
import harness.sql.typeclass.*
import zio.Chunk

final case class TableSchema[T[_[_]] <: Table](
    tableSchema: String,
    tableName: String,
    columns: T[Col],
    functorK: harness.sql.typeclass.FunctorK[T],
    flatten: harness.sql.typeclass.Flatten[T],
    codec: harness.sql.typeclass.QueryCodecMany[T[K11.Identity]],
    colChunk: Chunk[Col[?]],
    insertFragment: Fragment,
) {
  lazy val referenceName: String = s"$tableSchema.$tableName"
}
object TableSchema {

  type AnySchema = TableSchema[? <: ([_[_]] =>> harness.sql.Table)]

  inline def derived[T[_[_]] <: Table](tableSchema: String, tableName: String)(columns: T[Col])(implicit gen: K11.ProductGeneric[T]): TableSchema[T] = {
    val functorK: FunctorK[T] = FunctorK.derive[T]
    implicit val flatten: Flatten[T] = Flatten.derive[T]
    val codec: QueryCodecMany[T[K11.Identity]] =
      QueryCodecMany(
        QueryEncoderMany.forTable[T](functorK.mapK(columns) { [a] => (c: Col[a]) => c.codec.encoder }),
        QueryDecoderMany.forTable[T](columns),
      )
    val cols: Chunk[Col[?]] = flatten(columns)
    TableSchema[T](
      tableSchema = tableSchema,
      tableName = tableName,
      columns = columns,
      functorK = functorK,
      flatten = flatten,
      codec = codec,
      colChunk = cols,
      insertFragment = {
        val colsString = cols.map(_.colName).mkString(", ")
        val qMarks = cols.map(_ => "?").mkString(", ")
        Fragment(
          s"INSERT INTO $tableSchema.$tableName ($colsString) VALUES ($qMarks)", // RETURNING $colsString"
          {
            case Chunk(input)   => codec.encoder.encodeMany(input.asInstanceOf).zipWith(codec.encoder.klasses)(EncodedInputValue(_, _))
            case unmappedInputs => throw new RuntimeException(s"Insert query was passed weird data: ${unmappedInputs.mkString(", ")}")
          },
        )
      },
    )
  }

  inline def derived[T[_[_]] <: Table](tableName: String)(colInfo: T[Col])(implicit gen: K11.ProductGeneric[T]): TableSchema[T] =
    TableSchema.derived[T]("public", tableName)(colInfo)

}
