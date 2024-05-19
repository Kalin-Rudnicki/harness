package harness.testContainer.postgres

import harness.sql.*
import harness.sql.query.*
import harness.sql.typeclass.*
import harness.testContainer.*
import harness.zio.*
import harness.zio.test.*
import zio.*
import zio.test.*

object PostgresTestContainerSpec extends DefaultHarnessSpec {

  override def logLevel: Logger.LogLevel = Logger.LogLevel.Debug

  private val innerSpec: Spec[HarnessEnv & JDBCConnection, Any] =
    suite("PostgresTestContainerSpec")(
      test("can run a query") {
        for {
          res <- fr"SELECT 123".toQueryO[Int]("select-const")(using RowDecoder.fromColDecoder(ColDecoder.int))().single
        } yield assertTrue(res == 123)
      },
    )

  override def spec: TestSpec =
    innerSpec
      .provideSome[HarnessEnv & Scope & JDBCConnectionPool](
        JDBCConnection.poolLayer,
      )
      .provideSomeShared[HarnessEnv & Scope](
        PortFinder.layer(),
        PostgresTestContainer.layer,
        JDBCConnectionPool.configLayer,
      )

}
