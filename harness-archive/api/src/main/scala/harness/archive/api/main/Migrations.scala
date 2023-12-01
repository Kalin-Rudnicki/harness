package harness.archive.api.main

import harness.archive.api.db.model.LegacyTables.*
import harness.core.*
import harness.sql.autoSchema.*

object Migrations {

  val `0.0.1` =
    InMemoryMigration.auto(
      Version.parseUnsafe("0.0.1"),
      Tables.fromCompanions(user.V1, session.V1),
    )

  val `0.0.2` =
    InMemoryMigration.auto(
      Version.parseUnsafe("0.0.1"),
      Tables.fromCompanions(user.V1, session.V1, app.V1, log.V1, trace.V1),
    )

}
