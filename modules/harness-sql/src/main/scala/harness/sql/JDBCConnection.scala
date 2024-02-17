package harness.sql

import harness.sql.error.ConnectionError
import harness.zio.*
import java.sql.*
import zio.*

final class JDBCConnection(val jdbcConnection: java.sql.Connection) {

  def use[R, E, A](zio: ZIO[R & JDBCConnection, E, A]): ZIO[R, E, A] =
    zio.provideSomeEnvironment[R](_.add(this))

}
object JDBCConnection {

  val connectionFactoryLayer: ZLayer[ConnectionFactory & Logger & Scope, ConnectionError, JDBCConnection] =
    ZLayer.fromZIO {
      ZIO.service[ConnectionFactory].flatMap { cf =>
        Logger.log.debug("Acquiring db connection from ConnectionFactory") *>
          (cf.getJDBCConnection <* Logger.log.debug("Acquired db connection from ConnectionFactory"))
            .withFinalizerExit { (_, _) => Logger.log.debug("Releasing db connection from ConnectionFactory") }
      }
    }

  val poolLayer: ZLayer[JDBCConnectionPool & Logger & Scope, ConnectionError, JDBCConnection] =
    ZLayer.fromZIO {
      ZIO.service[JDBCConnectionPool].flatMap { cp =>
        Logger.log.debug("Acquiring db connection from ConnectionPool") *>
          (cp.pool.get <* Logger.log.debug("Acquired db connection from ConnectionPool"))
            .withFinalizerExit { (_, _) => Logger.log.debug("Releasing db connection from ConnectionPool") }
      }
    }

}
