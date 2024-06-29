package harness.sql

import cats.syntax.option.*
import harness.core.{given, *}
import harness.pk.*
import harness.sql.autoSchema.*
import harness.sql.query.*
import harness.testContainer.*
import harness.testContainer.postgres.PostgresTestContainer
import harness.zio.*
import harness.zio.test.*
import java.time.{Clock as _, *}
import java.util.UUID
import zio.*
import zio.test.*
import zio.test.Assertion.*

object SqlISpec extends DefaultHarnessSpec {

  // =====| Models |=====

  type TestTable1Id = TestTable1Id.Id
  object TestTable1Id extends TableKey

  final case class TestTable1[F[_]](
      id: F[TestTable1.Id],
      uuid: F[UUID],
      string: F[String],
      int: F[Int],
      boolean: F[Boolean],
      optUUID: F[Option[UUID]],
      optString: F[Option[String]],
      optInt: F[Option[Int]],
      optBoolean: F[Option[Boolean]],
  ) extends Table.WithId[F, TestTable1.Id]
  object TestTable1 extends Table.Companion.WithId[TestTable1Id, TestTable1] {

    override implicit lazy val tableSchema: TableSchema[TestTable1] =
      TableSchema.derived[TestTable1]("test_table_1") {
        new TestTable1.Cols(
          id = TestTable1.Id.pkCol,
          uuid = Col.uuid("uuid"),
          string = Col.string("string"),
          int = Col.int("int"),
          boolean = Col.boolean("boolean"),
          optUUID = Col.uuid("opt_uuid").optional,
          optString = Col.string("opt_string").optional,
          optInt = Col.int("opt_int").optional,
          optBoolean = Col.boolean("opt_boolean").optional,
        )
      }

  }

  type TestTable2Id = TestTable2Id.Id
  object TestTable2Id extends TableKey

  final case class TestTable2[F[_]](
      id: F[TestTable2.Id],
      localDateTime: F[LocalDateTime],
      localDate: F[LocalDate],
      localTime: F[LocalTime],
      offsetDateTime: F[OffsetDateTime],
      zonedDateTime: F[ZonedDateTime],
      instant: F[Instant],
  ) extends Table.WithId[F, TestTable2.Id]
  object TestTable2 extends Table.Companion.WithId[TestTable2Id, TestTable2] {

    override implicit lazy val tableSchema: TableSchema[TestTable2] =
      TableSchema.derived[TestTable2]("test_table_2") {
        new TestTable2.Cols(
          id = TestTable2.Id.pkCol,
          localDateTime = Col.localDateTime("local_date_time"),
          localDate = Col.localDate("local_date"),
          localTime = Col.localTime("local_time"),
          offsetDateTime = Col.offsetDateTime("offset_date_time"),
          zonedDateTime = Col.zonedDateTime("zoned_date_time"),
          instant = Col.instant("instant"),
        )
      }

  }

  // =====| Queries |=====

  object TestTable1Queries extends TableQueries[TestTable1Id, TestTable1] {

    val updateStrings: QueryI[TestTable1.Identity] =
      makeUpdate("test_table_1 - update-strings") {
        new TestTable1.Booleans(
          id = false,
          uuid = false,
          string = true,
          int = false,
          boolean = false,
          optUUID = false,
          optString = true,
          optInt = false,
          optBoolean = false,
        )
      }

    val selectByUUID: QueryIO[UUID, TestTable1.Identity] =
      Prepare.selectIO("test_table_1 - select-by-id") { Input[UUID] } { uuid =>
        Select
          .from[TestTable1]("t")
          .where { t => t.uuid === uuid }
          .returning { t => t }
      }

  }

  object TestTable2Queries extends TableQueries[TestTable2Id, TestTable2]

  // =====| Tests |=====

  private def gen2Table1s(uuid: UUID): UIO[(TestTable1.Identity, TestTable1.Identity)] =
    for {
      id1 <- TestTable1Id.genZio
      id2 <- TestTable1Id.genZio
      row1 = new TestTable1.Identity(
        id = id1,
        uuid = uuid,
        string = "3",
        int = 2,
        boolean = true,
        optUUID = uuid.some,
        optString = "3".some,
        optInt = 2.some,
        optBoolean = true.some,
      )
      row2 = new TestTable1.Identity(
        id = id2,
        uuid = uuid,
        string = "3",
        int = 2,
        boolean = true,
        optUUID = None,
        optString = None,
        optInt = None,
        optBoolean = None,
      )
    } yield (row1, row2)

  private val innerSpec: Spec[HarnessEnv & JDBCConnection, Any] =
    suite("SqlISpec")(
      suite("crud")(
        test("can insert") {
          for {
            uuid <- Random.nextUUID
            (row1, row2) <- gen2Table1s(uuid)
            _ <- TestTable1Queries.insert(row1).single
            _ <- TestTable1Queries.insert(row2).single
          } yield assertCompletes
        },
        test("can select") {
          for {
            uuid <- Random.nextUUID
            (row1, row2) <- gen2Table1s(uuid)
            id3 <- TestTable1Id.genZio
            _ <- TestTable1Queries.insert(row1).single
            _ <- TestTable1Queries.insert(row2).single
            res1 <- TestTable1Queries.selectById(row1.id).single
            res2 <- TestTable1Queries.selectById(row1.id).option
            res3 <- TestTable1Queries.selectById(row2.id).single
            res4 <- TestTable1Queries.selectById(row2.id).option
            res5 <- TestTable1Queries.selectById(id3).single.exit
            res6 <- TestTable1Queries.selectById(id3).option
            res7 <- TestTable1Queries.selectByUUID(uuid).chunk
            res8 <- TestTable1Queries.selectAll().chunk
          } yield assert(res1)(equalTo(row1)) &&
            assert(res2)(isSome(equalTo(row1))) &&
            assert(res3)(equalTo(row2)) &&
            assert(res4)(isSome(equalTo(row2))) &&
            assert(res5)(fails(anything)) &&
            assert(res6)(isNone) &&
            assert(res7)(hasSameElements(Chunk(row1, row2))) &&
            assert(res8.filter(_.uuid == uuid))(hasSameElements(Chunk(row1, row2)))
        },
        test("can delete") {
          for {
            uuid <- Random.nextUUID
            (row1, row2) <- gen2Table1s(uuid)
            _ <- TestTable1Queries.insert(row1).single
            _ <- TestTable1Queries.insert(row2).single
            res1 <- TestTable1Queries.selectById(row1.id).option
            res2 <- TestTable1Queries.selectById(row2.id).option
            _ <- TestTable1Queries.deleteById(row1.id).single
            _ <- TestTable1Queries.deleteById(row2.id).single
            res3 <- TestTable1Queries.selectById(row1.id).option
            res4 <- TestTable1Queries.selectById(row2.id).option
          } yield assert(res1)(isSome(equalTo(row1))) &&
            assert(res2)(isSome(equalTo(row2))) &&
            assert(res3)(isNone) &&
            assert(res4)(isNone)
        },
        test("can update") {
          for {
            uuid <- Random.nextUUID
            tmp <- gen2Table1s(uuid)
            (row1, row2) = tmp
            _ <- TestTable1Queries.insert(row1).single
            _ <- TestTable1Queries.insert(row2).single
            res1 <- TestTable1Queries.selectById(row1.id).single
            res2 <- TestTable1Queries.selectById(row2.id).single
            row1Updated: TestTable1.Identity = row1.copy(string = "updated", optString = "updated-2".some)
            _ <- TestTable1Queries.updateStrings(row1Updated.copy(int = 1234, optInt = 1234.some)).single
            res3 <- TestTable1Queries.selectById(row1.id).single
            res4 <- TestTable1Queries.selectById(row2.id).single
          } yield assertTrue(
            res1 == row1,
            res2 == row2,
            res3 == row1Updated,
            res4 == row2,
          )
        },
      ),
      suite("time")(
        test("works") {
          for {
            now <- Clock.currentDateTime
            zoned <- ZIO.attempt { now.atZoneSameInstant(ZoneId.of("Europe/Warsaw")) }
            id <- TestTable2Id.genZio
            row = new TestTable2.Identity(
              id = id,
              localDateTime = now.toLocalDateTime,
              localDate = now.toLocalDate,
              localTime = now.toLocalTime,
              offsetDateTime = now,
              zonedDateTime = zoned,
              instant = now.toInstant,
            )
            _ <- TestTable2Queries.insert(row).single
            res <- TestTable2Queries.selectById(row.id).single
            _ <- Logger.log.always(
              row.productElementNames
                .zip(row.productIterator)
                .zip(res.productIterator)
                .map { case ((n, a), b) =>
                  val str = s"$n: $a <-> $b"
                  if (a == b) str else str.red
                }
                .mkString("\n"),
            )
          } yield assertTrue( // TODO (KR) : assertTrue(res == row) , once time zone is figured out
            res.localDateTime == row.localDateTime,
            res.localDate == row.localDate,
            res.localTime == row.localTime,
            res.offsetDateTime.isEqual(row.offsetDateTime),
            res.zonedDateTime.isEqual(row.zonedDateTime),
            res.instant == row.instant,
            // catch when time zone is figured out
            res.offsetDateTime != row.offsetDateTime,
            res.zonedDateTime != row.zonedDateTime,
          )
        },
      ),
    )

  override def testSpec: TestSpec =
    innerSpec
      .provideSomeLayer[HarnessEnv & JDBCConnectionPool & Scope](
        JDBCConnection.poolLayer,
      )
      .provideSomeShared[HarnessEnv & Scope](
        PortFinder.layer(),
        PostgresTestContainer.layer,
        JDBCConnectionPool.configLayerWithMigrations(
          PlannedMigrations(
            InMemoryMigration.auto(Version.parseUnsafe("v0.0.1"), Tables.fromCompanions(TestTable1, TestTable2)),
          ),
        ),
      ) @@ TestAspect.withLiveClock @@ TestAspect.withLiveRandom

}
