package harness.archive.api.db.queries

import harness.archive.api.db.model as M
import harness.sql.*
import harness.sql.query.{given, *}

object Trace extends TableQueries[M.Trace.Id, M.Trace] {

  val byAppId: QueryIO[M.App.Id, M.Trace.Identity] =
    Prepare.selectIO("Trace - byAppName") { Input[M.App.Id] } { appId =>
      Select
        .from[M.Trace]("t")
        .where { t => t.appId === appId }
        .returning { t => t }
    }

  val deleteOutdated: QueryI[Long] =
    Prepare.deleteI("Trace - deleteOutdated") { Input[Long] } { now =>
      Delete
        .from[M.Trace]("t")
        .where { t => t.keepUntilEpochMS <= now }
    }

}
