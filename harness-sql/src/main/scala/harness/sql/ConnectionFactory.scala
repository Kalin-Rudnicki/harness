package harness.sql

import harness.zio.*
import java.sql.{Connection, DriverManager}
import zio.*

final class ConnectionFactory private (
    private[sql] val getJDBCConnection: HRIO[Scope, Connection],
)
object ConnectionFactory {

  private def wrapUnsafe(get: => Connection): ConnectionFactory =
    new ConnectionFactory(
      ZIO
        .acquireRelease(ZIO.hAttempt("Unable to get JDBC Connection")(get))
        .apply(con => ZIO.hAttempt("Unable to close JDBC connection")(con.close()).orDieH),
    )

  def apply(url: String): ConnectionFactory = wrapUnsafe(DriverManager.getConnection(url))
  def apply(url: String, user: String, password: String): ConnectionFactory = wrapUnsafe(DriverManager.getConnection(url, user, password))

}
