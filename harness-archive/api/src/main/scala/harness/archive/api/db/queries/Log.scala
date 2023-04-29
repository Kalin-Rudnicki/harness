package harness.archive.api.db.queries

import harness.archive.api.db.model as M
import harness.sql.*
import harness.sql.query.{given, *}

object Log extends TableQueries[M.Log.Id, M.Log] {

  val byAppId: QueryIO[M.App.Id, M.Log.Identity] =
    Prepare.selectIO("Log - byAppId") { Input[M.App.Id] } { appId =>
      Select
        .from[M.Log]("l")
        .where { l => l.appId === appId }
        .returning { l => l }
    }

  val deleteOutdated: QueryI[Long] =
    Prepare.deleteI("Log - deleteOutdated") { Input[Long] } { now =>
      Delete
        .from[M.Log]("l")
        .where { l => l.keepUntilEpochMS <= now }
    }

}
