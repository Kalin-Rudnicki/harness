package harness.sql

import harness.sql.query.QueryInputMapper
import harness.sql.typeclass.*
import harness.zio.*
import java.sql.*
import zio.*

final class JDBCConnection(val jdbcConnection: java.sql.Connection)
object JDBCConnection {

  val connectionFactoryLayer: HRLayer[ConnectionFactory & Logger & Scope, JDBCConnection] =
    ZLayer.fromZIO {
      ZIO.service[ConnectionFactory].flatMap { cf =>
        Logger.log.debug("Acquiring db connection from ConnectionFactory") *>
          (cf.getJDBCConnection <* Logger.log.debug("Acquired db connection from ConnectionFactory"))
            .withFinalizerExit { (_, _) => Logger.log.debug("Releasing db connection from ConnectionFactory") }
      }
    }

  val poolLayer: HRLayer[JDBCConnectionPool & Logger & Scope, JDBCConnection] =
    ZLayer.fromZIO {
      ZIO.service[JDBCConnectionPool].flatMap { cp =>
        Logger.log.debug("Acquiring db connection from ConnectionPool") *>
          (cp.pool.get <* Logger.log.debug("Acquired db connection from ConnectionPool"))
            .withFinalizerExit { (_, _) => Logger.log.debug("Releasing db connection from ConnectionPool") }
      }
    }

}
