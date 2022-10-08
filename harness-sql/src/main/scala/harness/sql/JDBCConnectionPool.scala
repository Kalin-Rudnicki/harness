package harness.sql

import zio.*

final class JDBCConnectionPool private (val pool: ZPool[Throwable, JDBCConnection])
object JDBCConnectionPool {

  def apply(cf: ConnectionFactory, min: Int, max: Int, duration: Duration): RIO[Scope, JDBCConnectionPool] =
    ZPool.make(cf.getJDBCConnection, min to max, duration).map(new JDBCConnectionPool(_))

  def layer(min: Int, max: Int, duration: Duration): RLayer[ConnectionFactory & Scope, JDBCConnectionPool] =
    ZLayer.fromZIO { ZIO.service[ConnectionFactory].flatMap(JDBCConnectionPool(_, min, max, duration)) }

}
