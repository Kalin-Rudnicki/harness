package harness.sql

import harness.sql.error.ConnectionError
import harness.zio.*
import zio.*

import java.sql.{Connection, DriverManager}

final class ConnectionFactory private (
    private[sql] val getJDBCConnection: ZIO[Scope, ConnectionError, JDBCConnection],
)
object ConnectionFactory {

  private def wrapUnsafe(get: => Connection): ConnectionFactory =
    new ConnectionFactory(ZIO.acquireAutoClosable(ZIO.attempt(get)).mapBoth(ConnectionError(_), JDBCConnection(_)))

  def apply(url: String): ConnectionFactory = wrapUnsafe(DriverManager.getConnection(url))
  def apply(url: String, user: String, password: String): ConnectionFactory = wrapUnsafe(DriverManager.getConnection(url, user, password))

}
