package harness.sql.autoSchema

import cats.data.NonEmptyList
import harness.core.Version
import harness.sql.*
import harness.zio.json.*

final case class Migration[F[_]](
    id: F[MigrationId],
    version: F[Version],
    steps: F[NonEmptyList[MigrationStep.Encoded]],
) extends Table.WithId[F, MigrationId]
object Migration extends Table.Companion.WithId[MigrationId, Migration] {

  override implicit lazy val tableSchema: TableSchema[Migration] =
    TableSchema.derived[Migration]("harness_auto_schema", "migration") {
      new Migration.Cols(
        id = Migration.Id.pkCol,
        version = Col.encoded[Version]("version"),
        steps = Col.encodedJson[NonEmptyList[MigrationStep.Encoded]]("steps"),
      )
    }

}

final case class InformationSchemaSchemata[F[_]](
    schemaName: F[String],
) extends Table
object InformationSchemaSchemata extends Table.Companion[InformationSchemaSchemata] {

  override implicit lazy val tableSchema: TableSchema[InformationSchemaSchemata] =
    TableSchema.derived[InformationSchemaSchemata]("information_schema", "schemata") {
      new InformationSchemaSchemata.Cols(
        schemaName = Col.string("schema_name"),
      )
    }

}
