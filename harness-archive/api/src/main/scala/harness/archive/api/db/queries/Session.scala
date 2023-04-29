package harness.archive.api.db.queries

import harness.archive.api.db.model as M
import harness.sql.*
import harness.sql.query.{given, *}

object Session extends TableQueries[M.Session.Id, M.Session] {

  val fromSessionToken: QueryIO[String, M.Session.Identity] =
    Prepare.selectIO(s"Session - fromSessionToken") { Input[String] } { token =>
      Select
        .from[M.Session]("s")
        .where { s => s.token === token }
        .returning { s => s }
    }

}
