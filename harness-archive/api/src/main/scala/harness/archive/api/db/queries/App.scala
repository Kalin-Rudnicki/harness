package harness.archive.api.db.queries

import harness.archive.api.db.model as M
import harness.sql.*
import harness.sql.query.{given, *}

object App extends TableQueries[M.App.Id, M.App] {

  val byName: QueryIO[String, M.App.Identity] =
    Prepare.selectIO("App - byName") { Input[String] } { appName =>
      Select
        .from[M.App]("a")
        .where { a => a.name === appName }
        .returning { a => a }
    }

}
