package harness.sql

import harness.sql.error.ConnectionError
import harness.zio.*
import zio.*

final class JDBCConnectionPool private (val pool: ZPool[ConnectionError, JDBCConnection])
object JDBCConnectionPool {

  def apply(cf: ConnectionFactory, min: Int, max: Int, duration: Duration): ZIO[Logger & Scope, ConnectionError, JDBCConnectionPool] =
    Logger.log.debug(s"Creating ConnectionPool($min, $max)") *>
      ZPool
        .make(cf.getJDBCConnection, min to max, duration)
        .map(new JDBCConnectionPool(_))
  // .withFinalizerExit { (_, _) => Logger.log.debug("Releasing ConnectionPool") }

  def layer(min: Int, max: Int, duration: Duration): ZLayer[ConnectionFactory & Logger & Scope, ConnectionError, JDBCConnectionPool] =
    ZLayer.fromZIO { ZIO.service[ConnectionFactory].flatMap(JDBCConnectionPool(_, min, max, duration)) }

  val configLayer: ZLayer[DbConfig & Logger & Scope, ConnectionError, JDBCConnectionPool] =
    ZLayer.fromZIO {
      ZIO.serviceWithZIO[DbConfig] { config =>
        JDBCConnectionPool(
          ConnectionFactory(config.psqlJdbcUrl, config.credentials.username, config.credentials.password),
          config.pool.minConnections,
          config.pool.maxConnections,
          config.pool.duration,
        )
      }
    }

}
