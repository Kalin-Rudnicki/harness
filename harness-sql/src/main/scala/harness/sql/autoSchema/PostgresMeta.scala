package harness.sql.autoSchema

import cats.syntax.either.*
import cats.syntax.option.*
import harness.sql
import harness.sql.*
import harness.sql.query.{given, *}
import harness.zio.*
import zio.*

object PostgresMeta {

  // =====| helpers |=====

  private def yesNo(colName: String): Col[Boolean] =
    Col
      .string(colName)
      .iemap {
        case "YES" => true.asRight
        case "NO"  => false.asRight
        case v     => s"Invalid value '$v'".leftNel
      } {
        case true  => "YES"
        case false => "NO"
      }

  private def tablesToMap(tables: Tables): Map[InformationSchemaTables.Identity, Chunk[(List[Col.Constraint], InformationSchemaColumns.Identity)]] =
    tables.tableSchemas.map { ti =>
      (
        new InformationSchemaTables.Identity(tableSchema = ti.tableSchema, tableName = ti.tableName),
        Chunk.fromIterable(
          ti.colList.map { col =>
            (
              col.constraints.reverse,
              new InformationSchemaColumns.Identity(
                tableSchema = ti.tableSchema,
                tableName = ti.tableName,
                columnName = col.colName,
                isNullable = col.nullable,
                dataType = col.colType,
              ),
            )
          },
        ),
      )
    }.toMap

  // =====| tables |=====

  final case class InformationSchemaTables[F[_]](
      tableSchema: F[String],
      tableName: F[String],
  ) extends Table
  object InformationSchemaTables extends Table.Companion[InformationSchemaTables] {

    override implicit lazy val tableSchema: TableSchema[InformationSchemaTables] =
      TableSchema.derived[InformationSchemaTables]("information_schema", "tables") {
        new InformationSchemaTables.Cols(
          tableSchema = Col.string("table_schema"),
          tableName = Col.string("table_name"),
        )
      }

  }

  final case class InformationSchemaColumns[F[_]](
      tableSchema: F[String],
      tableName: F[String],
      columnName: F[String],
      isNullable: F[Boolean],
      dataType: F[String],
  ) extends Table
  object InformationSchemaColumns extends Table.Companion[InformationSchemaColumns] {

    override implicit lazy val tableSchema: TableSchema[InformationSchemaColumns] =
      TableSchema.derived[InformationSchemaColumns]("information_schema", "columns") {
        new InformationSchemaColumns.Cols(
          tableSchema = Col.string("table_schema"),
          tableName = Col.string("table_name"),
          columnName = Col.string("column_name"),
          isNullable = yesNo("is_nullable"),
          dataType = Col.string("data_type").imap(_.toUpperCase)(_.toLowerCase),
        )
      }

  }

  // =====| queries |=====

  private val queryTablesAndColumns: QueryO[(InformationSchemaTables.Identity, Option[InformationSchemaColumns.Identity])] =
    Prepare
      .selectO {
        Select
          .from[InformationSchemaTables]("ist")
          .leftJoin[InformationSchemaColumns]("isc")
          .on { case (ist, isc) => (ist.tableSchema === isc.tableSchema) && (ist.tableName === isc.tableName) }
          .where { case (ist, _) => (ist.tableSchema !== Constant("pg_catalog")) && (ist.tableSchema !== Constant("information_schema")) }
          .orderBy { case (ist, _) => ist.tableName }
          .returning { case (ist, isc) => ist ~ isc }
      }

  // =====| effects |=====

  private val tablesAndColumns: HRIO[JDBCConnection & Logger, Map[InformationSchemaTables.Identity, Chunk[InformationSchemaColumns.Identity]]] =
    queryTablesAndColumns().groupByLeft(_._1)(_._2).chunk.map(_.toMap)

  object schemaDiff {

    def apply(tables: Tables): HRIO[JDBCConnection & Logger, Unit] =
      for {
        dbTAC <- tablesAndColumns
        schemaTAC = tablesToMap(tables)
        schemaIdx = tables.tableSchemas.map { ts => new InformationSchemaTables.Identity(ts.tableSchema, ts.tableName) }.zipWithIndex.toMap
        tablePairs = (dbTAC.keySet | schemaTAC.keySet).toList.sortBy(schemaIdx.getOrElse(_, 0)).map { k => (k, dbTAC.get(k), schemaTAC.get(k)) }
        _ <-
          ZIO.foreachDiscard(tablePairs) {
            case (t, Some(db), Some(schema)) =>
              val columnNames: Set[String] = db.map(_.columnName).toSet | schema.map(_._2.columnName).toSet
              val columnPairs: List[(Option[InformationSchemaColumns.Identity], Option[(List[Col.Constraint], InformationSchemaColumns.Identity)])] =
                columnNames.toList.map { c => (db.find(_.columnName == c), schema.find(_._2.columnName == c)) }
              val alterations: List[String] =
                columnPairs.flatMap {
                  case (Some(_), Some(_)) =>
                    None // TODO (KR) : this one is a bit trickier...
                  case (None, Some((constraints, col))) =>
                    s"ADD COLUMN ${col.columnName} ${col.dataType}${if (col.isNullable) "" else " NOT"} NULL${constraints.mkString(" ", " ", "")}".some
                  case (Some(col), None) =>
                    s"DROP COLUMN ${col.columnName}".some
                  case (None, None) =>
                    None
                }

              ZIO.when(alterations.nonEmpty) {
                val alterSql: String = s"ALTER TABLE ${t.tableSchema}.${t.tableName} ${alterations.mkString(", ")}"
                val query: Query = new Query(alterSql, QueryInputMapper.id)

                query().unit
              }
            case (t, None, Some(schema)) =>
              val createSql: String =
                schema
                  .map { case (constraints, col) =>
                    s"${col.columnName} ${col.dataType}${if (col.isNullable) "" else " NOT"} NULL${constraints.mkString(" ", " ", "")}"
                  }
                  .mkString(s"CREATE TABLE ${t.tableSchema}.${t.tableName} (", ", ", ")")
              val query: Query = new Query(createSql, QueryInputMapper.id)

              query().unit
            case (t, Some(_), None) =>
              val deleteSql: String = s"DROP TABLE ${t.tableSchema}.${t.tableName}"
              val query: Query = new Query(deleteSql, QueryInputMapper.id)

              query().unit
            case (_, None, None) =>
              ZIO.unit
          }
      } yield ()

    def withConnectionFactory(tables: Tables): RIO[ConnectionFactory & Logger, Unit] =
      ZIO.scoped { schemaDiff(tables).provideSomeLayer(JDBCConnection.connectionFactoryLayer) }

    def withPool(tables: Tables): RIO[JDBCConnectionPool & Logger, Unit] =
      ZIO.scoped { schemaDiff(tables).provideSomeLayer(JDBCConnection.poolLayer) }

  }

}
