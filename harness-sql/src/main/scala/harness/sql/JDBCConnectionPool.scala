package harness.sql

import harness.core.*
import harness.zio.*
import zio.*

final class JDBCConnectionPool private (val pool: ZPool[HError, JDBCConnection])
object JDBCConnectionPool {

  def apply(cf: ConnectionFactory, min: Int, max: Int, duration: Duration): HRIO[Scope, JDBCConnectionPool] =
    ZPool.make(cf.getJDBCConnection, min to max, duration).map(new JDBCConnectionPool(_))

  def layer(min: Int, max: Int, duration: Duration): HRLayer[ConnectionFactory & Scope, JDBCConnectionPool] =
    ZLayer.fromZIO { ZIO.service[ConnectionFactory].flatMap(JDBCConnectionPool(_, min, max, duration)) }

}
