package template.api.routes

import harness.core.*
import harness.sql.*
import harness.web.server.*
import harness.zio.*
import template.api.db.{model as M, queries as Q}
import zio.*

private[routes] object Helpers {

  // TODO (KR) : name this on a project specific basis
  val SessionToken: String = "Template-Session-Token"

  val userFromSession: HRIO[JDBCConnection & Logger & HttpRequest, M.User.Identity] =
    HttpRequest.cookie.get[String](Helpers.SessionToken).flatMap { tok =>
      Q.User.fromSessionToken(tok).single
    }

  val userFromSessionOptional: HRIO[JDBCConnection & Logger & HttpRequest, Option[M.User.Identity]] =
    HttpRequest.cookie.find[String](Helpers.SessionToken).flatMap {
      case Some(tok) => Q.User.fromSessionToken(tok).single.asSome
      case None      => ZIO.none
    }

}
