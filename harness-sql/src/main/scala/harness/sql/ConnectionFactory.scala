package harness.sql

import java.sql.{Connection, DriverManager}
import zio.*

final class ConnectionFactory private (
    private[sql] val getJDBCConnection: RIO[Scope, JDBCConnection],
)
object ConnectionFactory {

  private def wrapUnsafe(get: => Connection): ConnectionFactory =
    new ConnectionFactory(Utils.acquireClosable(get).map(JDBCConnection(_)))

  def apply(url: String): ConnectionFactory = wrapUnsafe(DriverManager.getConnection(url))
  def apply(url: String, user: String, password: String): ConnectionFactory = wrapUnsafe(DriverManager.getConnection(url, user, password))

}
