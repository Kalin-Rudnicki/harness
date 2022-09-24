package template.api.db.queries

import harness.sql.*
import harness.sql.query.{given, *}
import template.api.db.model as M

object User extends TableQueries[M.User.Id, M.User] {

  val fromSessionToken: QueryIO[String, M.User.Identity] =
    Prepare.selectIO { Input[String] } { token =>
      Select
        .from[M.Session]("s")
        .join[M.User]("u")
        .on { case (s, u) => s.userId === u.id }
        .where { case (s, _) => s.token === token }
        .returning { case (_, u) => u }
    }

  val byUsername: QueryIO[String, M.User.Identity] =
    Prepare
      .selectIO { Input[String] } { username =>
        Select
          .from[M.User]("u")
          .where { u => u.lowerUsername === username }
          .returning { u => u }
      }
      .cmap[String](_.toLowerCase)

}
