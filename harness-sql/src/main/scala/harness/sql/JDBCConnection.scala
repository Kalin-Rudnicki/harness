package harness.sql

import harness.sql.query.QueryInputMapper
import harness.sql.typeclass.*
import harness.zio.*
import java.sql.*
import zio.*

final class JDBCConnection(val jdbcConnection: java.sql.Connection)
object JDBCConnection {

  val connectionFactoryLayer: HRLayer[ConnectionFactory & Scope, JDBCConnection] =
    ZLayer.fromZIO { ZIO.service[ConnectionFactory].flatMap(_.getJDBCConnection) }

  val poolLayer: HRLayer[JDBCConnectionPool & Scope, JDBCConnection] =
    ZLayer.fromZIO { ZIO.service[JDBCConnectionPool].flatMap(_.pool.get) }

}
