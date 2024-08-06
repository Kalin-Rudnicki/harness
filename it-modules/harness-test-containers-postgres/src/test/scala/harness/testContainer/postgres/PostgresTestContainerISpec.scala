package harness.testContainer.postgres

import harness.sql.*
import harness.sql.query.*
import harness.sql.typeclass.*
import harness.testContainer.*
import harness.zio.*
import harness.zio.test.*
import zio.*
import zio.test.*

object PostgresTestContainerISpec extends HarnessSpec[Database] {

  override def layerProvider: LayerProvider[R] =
    LayerProvider.provideShared[Database](
      PortFinder.layer(),
      PostgresTestContainer.layer,
      Database.poolLayer,
    )

  override def testAspects: Chunk[TestSpecAspect] =
    Chunk(Logger.withLevel.debug.testAspect)

  override def testSpec: TestSpec =
    suite("PostgresTestContainerSpec")(
      test("can run a query") {
        for {
          res <- fr"SELECT 123".toQueryO[Int]("select-const").apply().single
        } yield assertTrue(res == 123)
      },
    )

}
