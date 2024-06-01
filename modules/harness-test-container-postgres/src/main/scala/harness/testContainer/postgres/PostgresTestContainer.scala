package harness.testContainer.postgres

import cats.syntax.option.*
import harness.sql.DbConfig
import harness.testContainer.*
import harness.zio.*
import zio.*

object PostgresTestContainer extends ContainerBuilder[PortFinder, DbConfig]("postgres") {

  protected final case class Internal(host: String, port: Int, database: String, username: String, password: String)

  override protected def makeInternal(metadata: ContainerBuilder.Metadata): RIO[HarnessEnv & PortFinder & Scope, Internal] =
    PortFinder.acquirePort("database").map(Internal("localhost", _, "test_container_database", "test-container-user", "test-container-password"))

  override protected def makeContainers(internal: Internal, metadata: ContainerBuilder.Metadata): RIO[HarnessEnv & PortFinder & Scope, List[TestContainer]] =
    ZIO.succeed {
      metadata
        .makeContainer("database", "postgres", "latest") {
          Sys
            .Command("psql", "-c", "SELECT 1 AS query_result;", "-h", internal.host, "-p", internal.port.toString, "-d", internal.database, "-U", internal.username)
            .addEnv("PGPASSWORD" -> internal.password)
            .executeString0(errLevel = Logger.LogLevel.Trace)
            .map { _.contains("query_result") }
        }
        .p(internal.port, 5432)
        .e("POSTGRES_DB", internal.database)
        .e("POSTGRES_USER", internal.username)
        .e("POSTGRES_PASSWORD", internal.password)
        :: Nil
    }

  override protected def makeEnv(internal: Internal, metadata: ContainerBuilder.Metadata): RIO[HarnessEnv & PortFinder & Scope, ZEnvironment[DbConfig]] =
    ZIO.attempt {
      ZEnvironment {
        DbConfig
          .Raw(
            target = DbConfig.Target(internal.database, internal.host.some, internal.port.some),
            credentials = DbConfig.Credentials(internal.username, internal.password),
            pool = DbConfig.PoolConfig(1, 16, 1.minute),
          )
          .toConfigUnsafe
      }
    }

}
