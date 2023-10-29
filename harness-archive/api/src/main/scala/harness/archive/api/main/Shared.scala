package harness.archive.api.main

import harness.sql.*
import harness.zio.*
import zio.*

object Shared {

  val poolLayer: HRLayer[Config & Logger & Scope, JDBCConnectionPool] =
    DbConfig.configLayer >>> JDBCConnectionPool.configLayer

}
