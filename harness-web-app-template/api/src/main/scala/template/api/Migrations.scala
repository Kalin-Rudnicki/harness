package template.api

import harness.core.*
import harness.sql.autoSchema.*
import template.api.db.model.LegacyTables.*

object Migrations {

  val `0.0.1` =
    InMemoryMigration.auto(Version.parseUnsafe("0.0.1"), Tables.fromCompanions(user.V1, session.V1))

  val `0.0.2` =
    InMemoryMigration(Version.parseUnsafe("0.0.2"), Tables.fromCompanions(user.V1, session.V1))(
      InMemoryMigration.Step(MigrationStep.CreateIndex(TableRef(user.V1.tableSchema), "lower_username_index", true, List("lower_username"))),
      InMemoryMigration.Step(MigrationStep.CreateIndex(TableRef(session.V1.tableSchema), "session_token_index", true, List("token"))),
    )

  val `0.0.3` =
    InMemoryMigration.auto(Version.parseUnsafe("0.0.3"), Tables.fromCompanions(user.V2, session.V1))

  val `0.0.4` =
    InMemoryMigration.auto(Version.parseUnsafe("0.0.4"), Tables.fromCompanions(user.V3, session.V1))

  val `0.0.5` =
    InMemoryMigration.auto(Version.parseUnsafe("0.0.5"), Tables.fromCompanions(user.V3, session.V1, paymentMethod.V1))

}
