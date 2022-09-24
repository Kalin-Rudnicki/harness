package template.api.db.queries

import harness.sql.*
import harness.sql.query.{given, *}
import template.api.db.model as M

object Session extends TableQueries[M.Session.Id, M.Session] {

  val fromSessionToken: QueryIO[String, M.Session.Identity] =
    Prepare.selectIO { Input[String] } { token =>
      Select
        .from[M.Session]("s")
        .where { s => s.token === token }
        .returning { s => s }
    }

}
