package harness.archive.api.main

import harness.sql.*
import harness.zio.*
import zio.*

object Shared {

  val poolLayer: HRLayer[Logger & Scope, JDBCConnectionPool] =
    ZLayer.fromZIO { JDBCConnectionPool(ConnectionFactory("jdbc:postgresql:archive", "kalin", "psql-pass"), 4, 16, Duration.fromSeconds(60)) }

}
