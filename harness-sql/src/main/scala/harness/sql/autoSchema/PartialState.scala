package harness.sql.autoSchema

import cats.data.EitherNel
import cats.syntax.either.*
import cats.syntax.parallel.*
import harness.sql.{Col, TableSchema}

final case class PartialState private (
    schemas: Map[SchemaRef, PartialState.Schema],
)
object PartialState {

  def apply(schemas: Map[SchemaRef, PartialState.Schema]): PartialState =
    if (schemas.contains(SchemaRef.Public)) new PartialState(schemas)
    else new PartialState(schemas.updated(SchemaRef.Public, PartialState.Schema(Map.empty)))

  def fromTables(tables: Tables): PartialState = {
    def fromTable(table: TableSchema.AnySchema): (TableRef, PartialState.Table) =
      (
        TableRef(SchemaRef(table.tableSchema), table.tableName),
        PartialState.Table(
          table.colList.map { col => (col.colName, PartialState.Column(col.colType, KeyType(col.keyType), col.nullable)) }.toMap,
        ),
      )

    PartialState(
      tables.tableSchemas
        .map(fromTable)
        .groupMap(_._1.schemaRef) { case (k, v) => (k.tableName, v) }
        .map { case (k, v) => (k, PartialState.Schema(v.toMap)) },
    )
  }

  def fromDbState(state: DbState): PartialState =
    PartialState(
      state.schemas.map { case (schemaRef, schema) =>
        schemaRef ->
          Schema(
            schema.tables.map { case (tableName, table) =>
              tableName ->
                Table(
                  table.columns.map { case (colName, col) =>
                    colName ->
                      Column(
                        col.colType,
                        col.keyType,
                        col.nullable,
                      )
                  },
                )
            },
          )
      },
    )

  final case class Schema(
      tables: Map[String, Table],
  )

  final case class Table(
      columns: Map[String, Column],
  )

  final case class Column(
      colType: Col.ColType,
      keyType: KeyType,
      nullable: Boolean,
  )

  // =====|  |=====

  object validate {

    def apply(state: PartialState, exp: PartialState): EitherNel[String, Unit] =
      (state.schemas.keySet | exp.schemas.keySet).toList.sortBy(_.schemaName).parTraverse_ { schemaName =>
        (state.schemas.get(schemaName), exp.schemas.get(schemaName)) match {
          case (Some(state), Some(exp)) => validateSchema(schemaName, state, exp)
          case (None, Some(_))          => s"Schema $schemaName is not in the correct state. Expected: exists, State: DNE.".leftNel
          case (Some(_), None)          => s"Schema $schemaName is not in the correct state. Expected: DNE, State: exists.".leftNel
          case (None, None)             => s"Internal defect (validateState): $state, $exp".leftNel
        }
      }

    private def validateSchema(ref: SchemaRef, state: PartialState.Schema, exp: PartialState.Schema): EitherNel[String, Unit] =
      (state.tables.keySet | exp.tables.keySet).toList.sorted.parTraverse_ { tableName =>
        (state.tables.get(tableName), exp.tables.get(tableName)) match {
          case (Some(state), Some(exp)) => validateTable(TableRef(ref, tableName), state, exp)
          case (None, Some(_))          => s"Table $ref.$tableName is not in the correct state. Expected: exists, State: DNE.".leftNel
          case (Some(_), None)          => s"Table $ref.$tableName is not in the correct state. Expected: DNE, State: exists.".leftNel
          case (None, None)             => s"Internal defect (validateSchema): $ref, $state, $exp".leftNel
        }
      }

    private def validateTable(ref: TableRef, state: PartialState.Table, exp: PartialState.Table): EitherNel[String, Unit] =
      (state.columns.keySet | exp.columns.keySet).toList.sorted.parTraverse_ { colName =>
        (state.columns.get(colName), exp.columns.get(colName)) match {
          case (Some(state), Some(exp)) => validateColumn(ColRef(ref.schemaRef, ref.tableName, colName), state, exp)
          case (None, Some(_))          => s"Column ${ref.schemaRef}.${ref.tableName} ($colName) is not in the correct state. Expected: exists, State: DNE.".leftNel
          case (Some(_), None)          => s"Column ${ref.schemaRef}.${ref.tableName} ($colName) is not in the correct state. Expected: DNE, State: exists.".leftNel
          case (None, None)             => s"Internal defect (validateTable): $ref, $state, $exp".leftNel
        }
      }

    private def validateColumn(ref: ColRef, state: PartialState.Column, exp: PartialState.Column): EitherNel[String, Unit] = {
      def compare[A](field: String, f: PartialState.Column => A): EitherNel[String, Unit] = {
        val stateValue = f(state)
        val expValue = f(exp)
        if (stateValue == expValue) ().asRight
        else s"Column ${ref.colName} in table ${ref.schemaRef}.${ref.tableName} is not in the correct state (field: $field). Expected: $expValue, State: $stateValue.".leftNel
      }

      (
        compare("colType", _.colType),
        compare("keyType", _.keyType),
        compare("nullable", _.nullable),
      ).parTupled.map(_ => ())
    }

  }

  object diff {

    def apply(state: PartialState, exp: PartialState, allowDrops: Boolean): EitherNel[String, List[MigrationStep.InMemory.Auto]] =
      (state.schemas.keySet | exp.schemas.keySet).toList
        .sortBy(_.schemaName)
        .parTraverse { schemaRef =>
          (state.schemas.get(schemaRef), exp.schemas.get(schemaRef)) match {
            case (Some(state), Some(exp)) => diffSchema(schemaRef, state, exp, allowDrops)
            case (Some(state), None) =>
              schemaRef match {
                case SchemaRef.Public =>
                  diffSchema(schemaRef, state, PartialState.Schema(Map.empty), allowDrops)
                case schemaRef: SchemaRef.Custom =>
                  if (allowDrops)
                    diffSchema(schemaRef, state, PartialState.Schema(Map.empty), allowDrops).map { MigrationStep.DropSchema(schemaRef) :: _ }
                  else s"Auto-drop disabled, unable to drop schema: $schemaRef".leftNel
              }
            case (None, Some(exp)) =>
              diffSchema(schemaRef, PartialState.Schema(Map.empty), exp, allowDrops).map { steps =>
                schemaRef match {
                  case SchemaRef.Public            => steps
                  case schemaRef: SchemaRef.Custom => MigrationStep.CreateSchema(schemaRef) :: steps
                }
              }
            case (None, None) => s"Internal defect (diffState): $schemaRef, $state, $exp".leftNel
          }
        }
        .map(_.flatten.sorted)

    private def diffSchema(ref: SchemaRef, state: PartialState.Schema, exp: PartialState.Schema, allowDrops: Boolean): EitherNel[String, List[MigrationStep.InMemory.Auto]] =
      (state.tables.keySet | exp.tables.keySet).toList.sorted
        .parTraverse { tableName =>
          val tableRef = TableRef(ref, tableName)
          (state.tables.get(tableName), exp.tables.get(tableName)) match {
            case (Some(state), Some(exp)) => diffTable(tableRef, state, exp, allowDrops)
            case (Some(state), None) =>
              if (allowDrops) diffTable(tableRef, state, PartialState.Table(Map.empty), allowDrops).map(MigrationStep.DropTable(tableRef) :: _)
              else s"Auto-drop disabled, unable to drop table: ${tableRef.schemaRef}.${tableRef.tableName}".leftNel
            case (None, Some(exp)) => diffTable(tableRef, PartialState.Table(Map.empty), exp, allowDrops).map(MigrationStep.CreateTable(tableRef) :: _)
            case (None, None)      => s"Internal defect (diffSchema): $tableRef, $state, $exp".leftNel
          }
        }
        .map(_.flatten)

    private def diffTable(ref: TableRef, state: PartialState.Table, exp: PartialState.Table, allowDrops: Boolean): EitherNel[String, List[MigrationStep.InMemory.Auto]] =
      (state.columns.keySet | exp.columns.keySet).toList.sorted
        .parTraverse { colName =>
          val colRef = ColRef(ref.schemaRef, ref.tableName, colName)
          (state.columns.get(colName), exp.columns.get(colName)) match {
            case (Some(state), Some(exp)) => diffColumn(colRef, state, exp)
            case (Some(state), None) =>
              if (allowDrops) (MigrationStep.DropCol(colRef, state.keyType) :: Nil).asRight
              else s"Auto-drop disabled, unable to drop col: ${colRef.colName} in table ${colRef.schemaRef}.${colRef.tableName}".leftNel
            case (None, Some(exp)) => (MigrationStep.CreateCol(colRef, exp.colType, exp.keyType, exp.nullable) :: Nil).asRight
            case (None, None)      => s"Internal defect (diffTable): $colRef, $state, $exp".leftNel
          }
        }
        .map(_.flatten)

    private def diffColumn(ref: ColRef, state: PartialState.Column, exp: PartialState.Column): EitherNel[String, List[MigrationStep.InMemory.Auto]] = {
      def compare[A](field: String, f: PartialState.Column => A): EitherNel[String, Unit] = {
        val stateValue = f(state)
        val expValue = f(exp)
        if (stateValue == expValue) ().asRight
        else s"Column ${ref.colName} in table ${ref.schemaRef}.${ref.tableName} is not in the correct state (field: $field). Expected: $expValue, State: $stateValue.".leftNel
      }

      (
        compare("colType", _.colType),
        compare("keyType", _.keyType),
      ).parTupled.map { _ =>
        (state.nullable, exp.nullable) match {
          case (true, true) | (false, false) => Nil
          case (true, false)                 => MigrationStep.SetColNotNullable(ref) :: Nil
          case (false, true)                 => MigrationStep.SetColNullable(ref) :: Nil
        }
      }
    }

  }

}
