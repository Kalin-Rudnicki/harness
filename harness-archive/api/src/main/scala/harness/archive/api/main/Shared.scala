package harness.archive.api.main

import harness.sql.*
import harness.zio.*
import zio.*

object Shared {

  val poolLayer: HRLayer[Logger & Scope, JDBCConnectionPool] =
    Config.layer.jarResource("application.conf.json") >>> DbConfig.configLayer >>> JDBCConnectionPool.configLayer

}
