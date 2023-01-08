package harness.sql

import harness.core.*
import harness.zio.*
import zio.*

final class JDBCConnectionPool private (val pool: ZPool[HError, JDBCConnection])
object JDBCConnectionPool {

  def apply(cf: ConnectionFactory, min: Int, max: Int, duration: Duration): HRIO[Logger & Scope, JDBCConnectionPool] =
    Logger.log.debug(s"Creating ConnectionPool($min, $max)") *>
      ZPool
        .make(cf.getJDBCConnection, min to max, duration)
        .map(new JDBCConnectionPool(_))
  // .withFinalizerExit { (_, _) => Logger.log.debug("Releasing ConnectionPool") }

  def layer(min: Int, max: Int, duration: Duration): HRLayer[ConnectionFactory & Logger & Scope, JDBCConnectionPool] =
    ZLayer.fromZIO { ZIO.service[ConnectionFactory].flatMap(JDBCConnectionPool(_, min, max, duration)) }

}
