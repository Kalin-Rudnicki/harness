package harness.archive.webServer

import harness.archive.db.model.LegacyTables.*
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
      Version.parseUnsafe("0.0.2"),
      Tables.fromCompanions(user.V1, session.V1, app.V1, log.V1, trace.V1),
    )

  val `0.0.3` =
    InMemoryMigration.auto(
      Version.parseUnsafe("0.0.3"),
      Tables.fromCompanions(user.V1, session.V1, app.V2, log.V1, trace.V1),
    )

  val `0.0.4` =
    InMemoryMigration.auto(
      Version.parseUnsafe("0.0.4"),
      Tables.fromCompanions(user.V1, session.V1, app.V2, log.V1, trace.V1, appToken.V1),
    )

}
