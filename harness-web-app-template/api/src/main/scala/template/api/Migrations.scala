package template.api

import harness.core.*
import harness.sql.autoSchema.*
import template.api.db.model.LegacyTables.*

object Migrations {

  val `0.0.1` =
    InMemoryMigration.auto(Version.parseUnsafe("0.0.1"), Tables.fromCompanions(user.V1, session.V1))

}
