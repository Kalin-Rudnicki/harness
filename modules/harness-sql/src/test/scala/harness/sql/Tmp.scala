package harness.sql

import cats.syntax.option.*
import harness.cli.*
import harness.core.*
import harness.pk.TableKey
import harness.sql.autoSchema.*
import harness.sql.error.QueryError
import harness.sql.query.{given, *}
import harness.sql.typeclass.*
import harness.zio.*
import java.time.{Clock as _, *}
import scala.annotation.unused
import scala.reflect.ClassTag
import shapeless3.deriving.*
import zio.*

object Tmp extends ExecutableApp {

  // =====|  |=====

  type MusicianId = MusicianId.Id
  object MusicianId extends TableKey

  type BandId = BandId.Id
  object BandId extends TableKey

  type MusicianInBandId = MusicianInBandId.Id
  object MusicianInBandId extends TableKey

  type NoteId = NoteId.Id
  object NoteId extends TableKey

  // =====|  |=====

  final case class Musician[F[_]](
      id: F[MusicianId],
      firstName: F[String],
      lastName: F[String],
      instrument: F[String],
      birthday: F[LocalDate],
      favoriteNumber: F[Option[Int]],
  ) extends Table.WithId[F, MusicianId]
  object Musician extends Table.Companion.WithId[MusicianId, Musician] {

    override implicit lazy val tableSchema: TableSchema[Musician] =
      TableSchema.derived[Musician]("tmp", "musician")(
        new Musician.Cols(
          id = Musician.Id.pkCol,
          firstName = Col.string("first_name"),
          lastName = Col.string("last_name"),
          instrument = Col.string("instrument"),
          birthday = Col.localDate("birthday"),
          favoriteNumber = Col.int("favorite_number").imap(_ - 10)(_ + 10).optional,
        ),
      )

  }

  final case class Band[F[_]](
      id: F[BandId],
      name: F[String],
      formationDate: F[LocalDate],
  ) extends Table.WithId[F, BandId]
  object Band extends Table.Companion.WithId[BandId, Band] {

    override implicit lazy val tableSchema: TableSchema[Band] =
      TableSchema.derived[Band]("tmp", "band")(
        new Band.Cols(
          id = Band.Id.pkCol,
          name = Col.string("name"),
          formationDate = Col.localDate("formation_date"),
        ),
      )

  }

  final case class MusicianInBand[F[_]](
      id: F[MusicianInBandId],
      musicianId: F[MusicianId],
      bandId: F[BandId],
      active: F[Boolean],
  ) extends Table.WithId[F, MusicianInBandId]
  object MusicianInBand extends Table.Companion.WithId[MusicianInBandId, MusicianInBand] {

    override implicit lazy val tableSchema: TableSchema[MusicianInBand] =
      TableSchema.derived[MusicianInBand]("tmp", "musician_in_band")(
        new MusicianInBand.Cols(
          id = MusicianInBand.Id.pkCol,
          musicianId = Musician.Id.fkCol("musician_id"),
          bandId = Band.Id.fkCol("band_id"),
          active = Col.boolean("active"),
        ),
      )

  }

  final case class Note[F[_]](
      id: F[NoteId],
      text: F[String],
      localDate: F[LocalDate],
      localTime: F[LocalTime],
      offsetTime: F[OffsetTime],
      localDateTime: F[LocalDateTime],
      offsetDateTime: F[OffsetDateTime],
  ) extends Table.WithId[F, NoteId]
  object Note extends Table.Companion.WithId[NoteId, Note] {

    override implicit lazy val tableSchema: TableSchema[Note] =
      TableSchema.derived[Note]("tmp", "note")(
        new Note.Cols(
          id = Note.Id.pkCol,
          text = Col.string("text"),
          localDate = Col.localDate("local_date"),
          localTime = Col.localTime("local_time"),
          offsetTime = Col.offsetTime("offset_time"),
          localDateTime = Col.localDateTime("local_date_time"),
          offsetDateTime = Col.offsetDateTime("offset_date_time"),
        ),
      )

  }

  // =====|  |=====

  object MusicianQueries extends TableQueries[Musician.Id, Musician] {

    val byNames: QueryIO[(String, String), Musician.Identity] =
      Prepare.selectIO("Musician - byNames")(Input[String] ~ Input[String]) { (first, last) =>
        Select
          .from[Musician]("m")
          .where { m => m.lastName === last && m.firstName === first }
          .returning { m => m }
      }

    val setFavoriteNumber: QueryIO[(Musician.Id, Option[Int]), Musician.Identity] =
      Prepare.updateIO("Musician - setFavoriteNumber")(Input[Musician.Id] ~ Input[Option[Int]]) { (id, fn) =>
        Update[Musician]("m")
          .where { m => m.id === id }
          .set { m => m.favoriteNumber := fn }
          .returning { m => m }
      }

    val withFavNumGreaterThan: QueryIO[Int, Musician.Identity] =
      Prepare.selectIO("Musician - withFavNumGreaterThan")(Input[Int]) { num =>
        Select
          .from[Musician]("m")
          .where { m => m.favoriteNumber > num }
          .returning { m => m }
      }

    val musicianAndBandNames: QueryO[(Musician.Identity, Option[String])] =
      Prepare.selectO("Musician - musicianAndBandNames") {
        Select
          .from[Musician]("m")
          .leftJoin[MusicianInBand]("mib")
          .on { (m, mib) => mib.musicianId === m.id }
          .leftJoin[Band]("b")
          .on { (_, mib, b) => mib.bandId === b.id }
          .where { (_, mib, _) => mib.active.isNull || mib.active }
          .orderBy { (m, _, _) => m.id }
          .returning { (m, _, b) => m ~ b.name }
      }

    val musicianAndBandNames2: QueryO[(Musician.Identity, Chunk[(String, Boolean)])] =
      Prepare.selectO("Musician - musicianAndBandNames2") {
        Select
          .from[Musician]("m")
          .returning { m =>
            m ~
              Select
                .from[MusicianInBand]("mib")
                .join[Band]("b")
                .on { (mib, b) => mib.bandId === b.id }
                .where { (mib, _) => mib.musicianId === m.id }
                .returningJson { (mib, b) => b.name.toMulti ~ mib.active }
                .chunk
          }
      }

  }

  object BandQueries extends TableQueries[Band.Id, Band] {

    val byName: QueryIO[String, Band.Identity] =
      Prepare.selectIO("Band - byName")(Input[String]) { name =>
        Select
          .from[Band]("b")
          .where { b => b.name === name }
          .returning { b => b }
      }

  }

  object MusicianInBandQueries extends TableQueries[MusicianInBand.Id, MusicianInBand]

  object NoteQueries extends TableQueries[Note.Id, Note] {

    val updateNoteText: QueryI[Note.Identity] =
      AutoQuery.update[Note](
        Note[Const[Boolean]](
          id = false,
          text = true,
          localDate = true,
          localTime = false,
          offsetTime = false,
          localDateTime = false,
          offsetDateTime = false,
        ),
      )

  }

  // =====|  |=====

  object Sample {

    def kalin: Musician.Identity =
      new Musician.Identity(
        id = Musician.Id.gen,
        firstName = "Kalin",
        lastName = "Rudnicki",
        instrument = "Coding",
        birthday = LocalDate.of(1998, 7, 5),
        favoriteNumber = 18.some,
      )

    def janine: Musician.Identity =
      new Musician.Identity(
        id = Musician.Id.gen,
        firstName = "Janine",
        lastName = "Rudnicki",
        instrument = "Homework",
        birthday = LocalDate.of(2001, 2, 19),
        favoriteNumber = 21.some,
      )

    def bob: Musician.Identity =
      new Musician.Identity(
        id = Musician.Id.gen,
        firstName = "Bob",
        lastName = "Rudnicki",
        instrument = "The News",
        birthday = LocalDate.of(1965, 12, 6),
        favoriteNumber = None,
      )

    def joy: Musician.Identity =
      new Musician.Identity(
        id = Musician.Id.gen,
        firstName = "Joy",
        lastName = "Rudnicki",
        instrument = "Piano",
        birthday = LocalDate.of(1967, 12, 20),
        favoriteNumber = 7.some,
      )

    def theBest: Band.Identity =
      new Band.Identity(
        id = Band.Id.gen,
        name = "The Best",
        formationDate = LocalDate.of(2000, 1, 1),
      )

    def anotherBand: Band.Identity =
      new Band.Identity(
        id = Band.Id.gen,
        name = "Another Band",
        formationDate = LocalDate.of(2020, 3, 15),
      )

  }

  private val randomNote: UIO[Note.Identity] =
    for {
      string <- Random.nextIntBetween('A'.toInt, 'Z'.toInt).map(_.toChar).replicateZIO(50).map(_.mkString)
      long <- Random.nextLongBetween(0, 1000000)
      offsetDateTime = OffsetDateTime.ofInstant(Instant.ofEpochSecond(long), ZoneId.systemDefault)
    } yield new Note.Identity(
      id = Note.Id.gen,
      text = string,
      localDate = offsetDateTime.toLocalDate,
      localTime = offsetDateTime.toLocalTime,
      offsetTime = offsetDateTime.toOffsetTime,
      localDateTime = offsetDateTime.toLocalDateTime,
      offsetDateTime = offsetDateTime,
    )

  @unused
  private def batchTimings(numIters: Int): ZIO[JDBCConnection & Logger & Telemetry, QueryError, Unit] =
    for {
      _ <- Logger.log.info(s"Generating sample sizes of ${numIters.toStringCommas}")

      batch1 <- randomNote.replicateZIO(numIters).map(Chunk.fromIterable)
      batch2 <- randomNote.replicateZIO(numIters).map(Chunk.fromIterable)

      _ <- Logger.log.info("Starting raw-inserts")
      _ <- ZIO.foreachDiscard(batch1)(NoteQueries.insert(_).single).telemetrize("raw-inserts", Logger.LogLevel.Info)
      _ <- ZIO.foreachDiscard(batch1)(n => NoteQueries.deleteById(n.id).single).telemetrize("raw-deletes", Logger.LogLevel.Info)
      _ <- Logger.log.info("Starting batch-inserts")
      _ <- NoteQueries.insert.batched(batch2).unit.telemetrize("batch-inserts", Logger.LogLevel.Info)
      _ <- NoteQueries.deleteById.batched(batch2.map(_.id)).expectSize(batch2.length).telemetrize("batch-deletes", Logger.LogLevel.Info)
    } yield ()

  @unused
  private def showMigration(migration: MigrationPlan): URIO[Logger, Unit] =
    Logger.log.info("") *>
      Logger.log.info(s"Migration ${migration.version}") *>
      ZIO.foreachDiscard(migration.steps.toList) { Logger.log.info(_) }

  override val executable: Executable =
    Executable
      .withParser(Parser.unit)
      .withLayer {
        HConfig.layer.append.jsonString("""{
            |  "db": {
            |    "target": {
            |      "database": "tmp"
            |    },
            |    "credentials": {
            |      "username": "kalin",
            |      "password": "psql-pass"
            |    },
            |    "pool": {
            |      "minConnections": 4,
            |      "maxConnections": 16,
            |      "duration": "P60S"
            |    }
            |  }
            |}""".stripMargin) >+>
          DbConfig.configLayer >+>
          JDBCConnectionPool.configLayer >>>
          JDBCConnection.poolLayer
      }
      .withEffect {
        for {
          _ <- Logger.log.info("Starting...")
          _ <- Logger.log.debug("Starting[Debug]")
          tables = Tables(Musician.tableSchema, Band.tableSchema, MusicianInBand.tableSchema, Note.tableSchema)
          // _ <- PostgresMeta.schemaDiff(tables)

          // _ <- batchTimings(1000)

          // initialNote <- randomNote
          // _ <- NoteQueries.insert(initialNote).single
          // _ <- NoteQueries.updateNoteText(initialNote.copy(text = s"updated - ${initialNote.text}", localDate = initialNote.localDate.plusDays(1))).single
          // updatedNote <- NoteQueries.selectById(initialNote.id).single
          // _ <- Logger.log.info(s"$initialNote\n$updatedNote")

          _ <- MigrationRunner.runMigrations(
            PlannedMigrations(
              InMemoryMigration.auto(Version.parseUnsafe("0.0.1"), tables),
            ),
          )
        } yield ()
      }

  private implicit val errorLogger: ErrorLogger[Throwable] =
    ErrorLogger.withGetMessage[Throwable].atLevel.fatal

}
