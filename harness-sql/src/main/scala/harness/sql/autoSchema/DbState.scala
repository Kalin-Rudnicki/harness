package harness.sql.autoSchema

import cats.syntax.either.*
import harness.sql.Col

final case class DbState(
    schemas: Map[SchemaRef, DbState.Schema],
    indexesTable: Map[String, TableRef],
) { self =>

  def schema(schemaRef: SchemaRef): Either[String, DbState.Schema] =
    self.schemas.get(schemaRef).toRight(s"No such schema: '$schemaRef'")

  def index(name: String): Either[String, TableRef] =
    indexesTable.get(name).toRight(s"No such index: '$name'")

  def withMappedKeyTypes(f: KeyType => KeyType): DbState =
    copy(schemas = schemas.map((name, schema) => (name, schema.withMappedKeyTypes(f))))

}
object DbState {

  val initial: DbState =
    DbState(Map(SchemaRef.Public -> Schema.initial(SchemaRef.Public)), Map.empty)

  final case class Schema(
      name: String,
      tables: Map[String, Table],
  ) {

    def table(name: String): Either[String, Table] =
      tables.get(name).toRight(s"No such table: '$name'")

    def withMappedKeyTypes(f: KeyType => KeyType): Schema =
      copy(tables = tables.map((name, table) => (name, table.withMappedKeyTypes(f))))

  }
  object Schema {
    def initial(ref: SchemaRef): Schema = Schema(ref.schemaName, Map.empty)
  }

  final case class Table(
      name: String,
      columns: Map[String, Column],
      indexes: Map[String, Index],
  ) {

    def column(name: String): Either[String, Column] =
      columns.get(name).toRight(s"No such column: '$name'")

    def index(name: String): Either[String, Index] =
      indexes.get(name).toRight(s"No such index: '$name'")

    def withMappedKeyTypes(f: KeyType => KeyType): Table =
      copy(columns = columns.map((name, col) => (name, col.copy(keyType = f(col.keyType)))))

  }
  object Table {
    def initial(ref: TableRef): Table = Table(ref.tableName, Map.empty, Map.empty)
  }

  final case class Column(
      name: String,
      colType: Col.ColType,
      keyType: KeyType,
      nullable: Boolean,
  )

  final case class Index(
      name: String,
      unique: Boolean,
      cols: List[String],
  )

  private def mapKeyType(keyType: KeyType, oldColRef: ColRef, newColName: String): KeyType =
    keyType match {
      case KeyType.ForeignKey(`oldColRef`) => KeyType.ForeignKey(ColRef(oldColRef.schemaRef, oldColRef.tableName, newColName))
      case _                               => keyType
    }
  private def mapKeyType(keyType: KeyType, oldTableRef: TableRef, newTableName: String): KeyType =
    keyType match {
      case KeyType.ForeignKey(ColRef(oldTableRef.schemaRef, oldTableRef.tableName, colName)) => KeyType.ForeignKey(ColRef(oldTableRef.schemaRef, newTableName, colName))
      case _                                                                                 => keyType
    }
  private def mapKeyType(keyType: KeyType, oldSchemaRef: SchemaRef, newSchemaRef: SchemaRef): KeyType =
    keyType match {
      case KeyType.ForeignKey(ColRef(`oldSchemaRef`, tableName, colName)) => KeyType.ForeignKey(ColRef(newSchemaRef, tableName, colName))
      case _                                                              => keyType
    }

  def next(state: DbState, step: MigrationStep.InMemory): Either[String, (MigrationEffect, MigrationStep.Encoded, DbState)] =
    step match {
      case step @ MigrationStep.CreateSchema(ref) =>
        for {
          _ <- Either.cond(!state.schemas.contains(ref), (), s"Schema '$ref' already exists")
          newState = DbState(state.schemas.updated(ref, Schema.initial(ref)), state.indexesTable)
        } yield (MigrationEffect.Sql(s"CREATE SCHEMA $ref"), step, newState)
      case step @ MigrationStep.RenameSchema(refBefore, refAfter) =>
        for {
          schema <- state.schema(refBefore)
          _ <- Either.cond(!state.schemas.contains(refAfter), (), s"Schema '$refAfter' already exists")
          newSchema = Schema(refAfter.schemaName, schema.tables)
          newIndexes = state.indexesTable.toList.map { case (name, TableRef(_, tableName)) => (name, TableRef(refAfter, tableName)) }.toMap
          newState = DbState(state.schemas.removed(refBefore).updated(refAfter, newSchema), newIndexes).withMappedKeyTypes(mapKeyType(_, refBefore, refAfter))
        } yield (MigrationEffect.Sql(s"ALTER SCHEMA $refBefore RENAME TO $refAfter"), step, newState)
      case step @ MigrationStep.DropSchema(ref) =>
        for {
          schema <- state.schema(ref)
          _ <- Either.cond(schema.tables.isEmpty, (), s"Can not drop schema '$ref', it still has tables: ${schema.tables.keySet.mkString(", ")}")
          newState = DbState(state.schemas.removed(ref), state.indexesTable)
        } yield (MigrationEffect.Sql(s"DROP SCHEMA $ref"), step, newState)
      case step @ MigrationStep.CreateTable(ref) =>
        for {
          schema <- state.schema(ref.schemaRef)
          _ <- Either.cond(!schema.tables.contains(ref.tableName), (), s"Table '${ref.tableName}' already exists in schema ${ref.schemaRef}")
          newSchema = Schema(schema.name, schema.tables.updated(ref.tableName, Table.initial(ref)))
          newState = DbState(state.schemas.updated(ref.schemaRef, newSchema), state.indexesTable)
        } yield (MigrationEffect.Sql(s"CREATE TABLE ${ref.schemaRef}.${ref.tableName}()"), step, newState)
      case step @ MigrationStep.RenameTable(schemaRef, nameBefore, nameAfter) =>
        for {
          schema <- state.schema(schemaRef)
          table <- schema.table(nameBefore)
          _ <- Either.cond(!schema.tables.contains(nameAfter), (), s"Table '$nameAfter' already exists in schema $schemaRef")
          newTable = Table(nameAfter, table.columns, table.indexes)
          newSchema = Schema(schema.name, schema.tables.removed(nameBefore).updated(nameAfter, newTable))
          newIndexes = state.indexesTable.toList.map { case (name, TableRef(schemaRef, _)) => (name, TableRef(schemaRef, nameAfter)) }.toMap
          newState = DbState(state.schemas.updated(schemaRef, newSchema), newIndexes).withMappedKeyTypes(mapKeyType(_, TableRef(schemaRef, nameBefore), nameAfter))
        } yield (MigrationEffect.Sql(s"ALTER TABLE $schemaRef.$nameBefore RENAME TO $nameAfter"), step, newState)
      case step @ MigrationStep.DropTable(ref) =>
        for {
          schema <- state.schema(ref.schemaRef)
          _ <- schema.table(ref.tableName)
          newSchema = Schema(schema.name, schema.tables.removed(ref.tableName))
          newIndexes = state.indexesTable.filter(_._2 == ref)
          newState = DbState(state.schemas.updated(ref.schemaRef, newSchema), newIndexes)
        } yield (MigrationEffect.Sql(s"DROP TABLE ${ref.schemaRef}.${ref.tableName}"), step, newState)
      case step @ MigrationStep.CreateCol(ref, colType, keyType, nullable) =>
        for {
          schema <- state.schema(ref.schemaRef)
          table <- schema.table(ref.tableName)
          _ <- Either.cond(!table.columns.contains(ref.colName), (), s"Column '${ref.colName}' already exists in table ${ref.schemaRef}.${ref.tableName}")
          col = Column(ref.colName, colType, keyType, nullable)
          (removeIndexes, keepIndexes) = table.indexes.toList.partitionMap { case (name, index) =>
            Either.cond(index.cols.contains(ref.colName), index, name)
          }
          newTable = Table(table.name, table.columns.updated(ref.colName, col), keepIndexes.map(idx => (idx.name, idx)).toMap)
          newSchema = Schema(schema.name, schema.tables.updated(newTable.name, newTable))
          newIndexes = state.indexesTable.filterNot((name, _) => removeIndexes.contains(name))
          newState = DbState(state.schemas.updated(SchemaRef(newSchema.name), newSchema), newIndexes)
          nullableSql = if (nullable) "NULL" else "NOT NULL"
          sql = s"ALTER TABLE ${ref.schemaRef}.${ref.tableName} ADD COLUMN ${ref.colName} $colType $nullableSql${keyType.sql}"
        } yield (MigrationEffect.Sql(sql), step, newState)
      case step @ MigrationStep.RenameCol(tableRef, nameBefore, nameAfter) =>
        for {
          schema <- state.schema(tableRef.schemaRef)
          table <- schema.table(tableRef.tableName)
          col <- table.column(nameBefore)
          _ <- Either.cond(!table.columns.contains(nameAfter), (), s"Column '$nameAfter' already exists in table ${tableRef.schemaRef}.${tableRef.tableName}")
          newCol = col.copy(name = nameAfter)
          newIndexes = table.indexes.map { case (name, index) => (name, index.copy(cols = index.cols.map { col => if (col == nameBefore) nameAfter else col })) }
          newTable = Table(table.name, table.columns.removed(nameBefore).updated(newCol.name, newCol), newIndexes)
          newSchema = Schema(schema.name, schema.tables.updated(newTable.name, newTable))
          newState = DbState(state.schemas.updated(SchemaRef(newSchema.name), newSchema), state.indexesTable)
            .withMappedKeyTypes(mapKeyType(_, ColRef(tableRef.schemaRef, tableRef.tableName, nameBefore), nameAfter))
        } yield (MigrationEffect.Sql(s"ALTER TABLE ${tableRef.schemaRef}.${tableRef.tableName} RENAME COLUMN $nameBefore TO $nameAfter"), step, newState)
      case step @ MigrationStep.DropCol(ref, _) =>
        for {
          schema <- state.schema(ref.schemaRef)
          table <- schema.table(ref.tableName)
          _ <- table.column(ref.colName)
          _ <- table.indexes.values.find(_.cols.contains(ref.colName)).map(idx => s"Column '${ref.colName}' is referenced by index ${idx.name}").toLeft(())
          newTable = table.copy(columns = table.columns.removed(ref.colName))
          newSchema = schema.copy(tables = schema.tables.updated(newTable.name, newTable))
          newState = state.copy(schemas = state.schemas.updated(SchemaRef(newSchema.name), newSchema))
        } yield (MigrationEffect.Sql(s"ALTER TABLE ${ref.schemaRef}.${ref.tableName} DROP COLUMN ${ref.colName}"), step, newState)
      case step @ MigrationStep.SetColNotNullable(ref) =>
        for {
          schema <- state.schema(ref.schemaRef)
          table <- schema.table(ref.tableName)
          col <- table.column(ref.colName)
          _ <- Either.cond(!col.nullable, (), s"Column ${ref.colName} is already not nullable")
          newCol = col.copy(nullable = false)
          newTable = table.copy(columns = table.columns.updated(newCol.name, newCol))
          newSchema = schema.copy(tables = schema.tables.updated(newTable.name, newTable))
          newState = state.copy(schemas = state.schemas.updated(SchemaRef(newSchema.name), newSchema))
        } yield (MigrationEffect.Sql(s"ALTER TABLE ${ref.schemaRef}.${ref.tableName} ALTER COLUMN ${ref.colName} SET NOT NULL"), step, newState)
      case step @ MigrationStep.SetColNullable(ref) =>
        for {
          schema <- state.schema(ref.schemaRef)
          table <- schema.table(ref.tableName)
          col <- table.column(ref.colName)
          _ <- Either.cond(col.nullable, (), s"Column ${ref.colName} is already nullable")
          newCol = col.copy(nullable = true)
          newTable = table.copy(columns = table.columns.updated(newCol.name, newCol))
          newSchema = schema.copy(tables = schema.tables.updated(newTable.name, newTable))
          newState = state.copy(schemas = state.schemas.updated(SchemaRef(newSchema.name), newSchema))
        } yield (MigrationEffect.Sql(s"ALTER TABLE ${ref.schemaRef}.${ref.tableName} ALTER COLUMN ${ref.colName} DROP NOT NULL"), step, newState)
      case step @ MigrationStep.CreateIndex(tableRef, name, unique, cols) =>
        for {
          _ <- Either.cond(!state.indexesTable.contains(name), (), s"Index with name '$name' already exists")
          schema <- state.schema(tableRef.schemaRef)
          table <- schema.table(tableRef.tableName)
          _ <- cols.find(!table.columns.contains(_)).map(col => s"No such column to index on: $col").toLeft(())
          newIndex = Index(name, unique, cols)
          newTable = table.copy(indexes = table.indexes.updated(newIndex.name, newIndex))
          newSchema = schema.copy(tables = schema.tables.updated(newTable.name, newTable))
          newState = DbState(state.schemas.updated(SchemaRef(newSchema.name), newSchema), state.indexesTable.updated(name, tableRef))
          uniqueSql = if (unique) s" UNIQUE" else ""
          sql = s"CREATE$uniqueSql INDEX $name ON ${tableRef.schemaRef}.${tableRef.tableName} (${cols.mkString(", ")})"
        } yield (MigrationEffect.Sql(sql), step, newState)
      case step @ MigrationStep.RenameIndex(nameBefore, nameAfter) =>
        for {
          tableRef <- state.index(nameBefore)
          schema <- state.schema(tableRef.schemaRef)
          table <- schema.table(tableRef.tableName)
          index <- table.index(nameBefore)
          _ <- Either.cond(!state.indexesTable.contains(nameAfter), (), s"Index with name '$nameAfter' already exists")
          newIndex = index.copy(name = nameAfter)
          newTable = table.copy(indexes = table.indexes.removed(nameBefore).updated(newIndex.name, newIndex))
          newSchema = schema.copy(tables = schema.tables.updated(newTable.name, newTable))
          newIndexes = state.indexesTable.removed(nameBefore).updated(newIndex.name, tableRef)
          newState = DbState(state.schemas.updated(SchemaRef(newSchema.name), newSchema), newIndexes)
        } yield (MigrationEffect.Sql(s"ALTER INDEX $nameBefore RENAME TO $nameAfter"), step, newState)
      case step @ MigrationStep.DropIndex(name) =>
        for {
          tableRef <- state.index(name)
          schema <- state.schema(tableRef.schemaRef)
          table <- schema.table(tableRef.tableName)
          newTable = table.copy(indexes = table.indexes.removed(name))
          newSchema = schema.copy(tables = schema.tables.updated(newTable.name, newTable))
          newIndexes = state.indexesTable.removed(name)
          newState = DbState(state.schemas.updated(SchemaRef(newSchema.name), newSchema), newIndexes)
        } yield (MigrationEffect.Sql(s"DROP INDEX $name"), step, newState)
      case MigrationStep.InMemory.Code(name, up, down) =>
        (MigrationEffect.Code(name, up), MigrationStep.Encoded.Code(name, down.nonEmpty), state).asRight
    }

}
