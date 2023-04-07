package harness.sql

import cats.~>
import cats.syntax.option.*
import harness.cli.*
import harness.core.*
import harness.sql.autoSchema.*
import harness.sql.query.{given, *}
import harness.sql.typeclass.*
import harness.zio.*
import java.time.{Clock as _, *}
import java.util.{TimeZone, UUID}
import shapeless3.deriving.*
import zio.*

object Tmp extends ExecutableApp {

  // =====|  |=====

  final case class Musician[F[_]](
      id: F[Musician.Id],
      firstName: F[String],
      lastName: F[String],
      instrument: F[String],
      birthday: F[LocalDate],
      favoriteNumber: F[Option[Int]],
  ) extends Table.WithId[F, Musician.Id]
  object Musician extends Table.Companion.WithId[Musician] {

    override implicit lazy val tableSchema: TableSchema[Musician] =
      TableSchema.derived[Musician]("musician")(
        Musician[Col](
          id = Id.pkCol,
          firstName = Col.string("first_name"),
          lastName = Col.string("last_name"),
          instrument = Col.string("instrument"),
          birthday = Col.localDate("birthday"),
          favoriteNumber = Col.int("favorite_number").imap(_ - 10)(_ + 10).optional,
        ),
      )

  }

  final case class Band[F[_]](
      id: F[Band.Id],
      name: F[String],
      formationDate: F[LocalDate],
  ) extends Table.WithId[F, Band.Id]
  object Band extends Table.Companion.WithId[Band] {

    override implicit lazy val tableSchema: TableSchema[Band] =
      TableSchema.derived[Band]("band")(
        Band[Col](
          id = Id.pkCol,
          name = Col.string("name"),
          formationDate = Col.localDate("formation_date"),
        ),
      )

  }

  final case class MusicianInBand[F[_]](
      id: F[MusicianInBand.Id],
      musicianId: F[Musician.Id],
      bandId: F[Band.Id],
      active: F[Boolean],
  ) extends Table.WithId[F, MusicianInBand.Id]
  object MusicianInBand extends Table.Companion.WithId[MusicianInBand] {

    override implicit lazy val tableSchema: TableSchema[MusicianInBand] =
      TableSchema.derived[MusicianInBand]("musician_in_band")(
        MusicianInBand[Col](
          id = Id.pkCol,
          musicianId = Musician.Id.fkCol("musician_id"),
          bandId = Band.Id.fkCol("band_id"),
          active = Col.boolean("active"),
        ),
      )

  }

  final case class Note[F[_]](
      id: F[Note.Id],
      text: F[String],
      localDate: F[LocalDate],
      localTime: F[LocalTime],
      offsetTime: F[OffsetTime],
      localDateTime: F[LocalDateTime],
      offsetDateTime: F[OffsetDateTime],
  ) extends Table.WithId[F, Note.Id]
  object Note extends Table.Companion.WithId[Note] {

    override implicit lazy val tableSchema: TableSchema[Note] =
      TableSchema.derived[Note]("note")(
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

  object NoteQueries extends TableQueries[Note.Id, Note]

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

  override val executable: Executable =
    Executable
      .withParser(Parser.unit)
      .withLayer {
        ZLayer.succeed(ConnectionFactory("jdbc:postgresql:postgres", "kalin", "psql-pass")) >+>
          JDBCConnection.connectionFactoryLayer
      }
      .withEffect {
        for {
          _ <- Logger.log.info("Starting...")
          _ <- PostgresMeta.schemaDiff(Tables(Musician.tableSchema, Band.tableSchema, MusicianInBand.tableSchema, Note.tableSchema))

          numIters = 12500
          _ <- Logger.log.info(s"Generating sample sizes of ${numIters.toStringCommas}")

          batch1 <- randomNote.replicateZIO(numIters).map(Chunk.fromIterable)
          batch2 <- randomNote.replicateZIO(numIters).map(Chunk.fromIterable)

          _ <- Logger.log.info("Starting raw-inserts")
          _ <- ZIO.foreachDiscard(batch1)(NoteQueries.insert(_).single).trace("raw-inserts", Logger.LogLevel.Info)
          _ <- ZIO.foreachDiscard(batch1)(n => NoteQueries.deleteById(n.id).single).trace("raw-deletes", Logger.LogLevel.Info)
          _ <- Logger.log.info("Starting batch-inserts")
          _ <- NoteQueries.insert.batched(batch2).unit.trace("batch-inserts", Logger.LogLevel.Info)
          _ <- NoteQueries.deleteById.batched(batch2.map(_.id)).expectSize(batch2.length).trace("batch-deletes", Logger.LogLevel.Info)
        } yield ()
      }

}
