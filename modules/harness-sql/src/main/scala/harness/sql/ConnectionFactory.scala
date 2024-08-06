package harness.sql

import harness.sql.error.ConnectionError
import harness.zio.*
import java.sql.{Connection, DriverManager}
import zio.*

final class ConnectionFactory private (
    private[sql] val getJDBCConnection: ZIO[Scope, ConnectionError, JDBCConnection],
)
object ConnectionFactory {

  private def wrapUnsafe(get: => Connection): ConnectionFactory =
    new ConnectionFactory(
      for {
        con <- ZIO.attempt(get).autoClose.mapError(ConnectionError(_))
        id <- Random.nextUUID
      } yield JDBCConnection(con, id),
    )

  def apply(url: String): ConnectionFactory = wrapUnsafe(DriverManager.getConnection(url))
  def apply(url: String, user: String, password: String): ConnectionFactory = wrapUnsafe(DriverManager.getConnection(url, user, password))
  def apply(dbConfig: DbConfig): ConnectionFactory = ConnectionFactory(dbConfig.psqlJdbcUrl, dbConfig.credentials.username, dbConfig.credentials.password)

}
