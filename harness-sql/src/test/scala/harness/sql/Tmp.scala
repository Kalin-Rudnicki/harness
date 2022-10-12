package harness.sql

import cats.~>
import cats.syntax.option.*
import harness.cli.*
import harness.core.*
import harness.sql.autoSchema.*
import harness.sql.query.{given, *}
import harness.sql.typeclass.*
import harness.zio.*
import java.time.*
import java.util.UUID
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
          birthday = Col.date("birthday"),
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
          formationDate = Col.date("formation_date"),
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

  // =====|  |=====

  object MusicianQueries extends TableQueries[Musician.Id, Musician] {

    val byNames: QueryIO[(String, String), Musician.Identity] =
      Prepare.selectIO(Input[String] ~ Input[String]) { (first, last) =>
        Select
          .from[Musician]("m")
          .where { m => m.lastName === last && m.firstName === first }
          .returning { m => m }
      }

    val setFavoriteNumber: QueryIO[(Musician.Id, Option[Int]), Musician.Identity] =
      Prepare.updateIO(Input[Musician.Id] ~ Input[Option[Int]]) { (id, fn) =>
        Update[Musician]("m")
          .where { m => m.id === id }
          .set { m => m.favoriteNumber := fn }
          .returning { m => m }
      }

    val withFavNumGreaterThan: QueryIO[Int, Musician.Identity] =
      Prepare.selectIO(Input[Int]) { num =>
        Select
          .from[Musician]("m")
          .where { m => m.favoriteNumber > num }
          .returning { m => m }
      }

    val musicianAndBandNames: QueryO[(Musician.Identity, Option[String])] =
      Prepare.selectO {
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
      Prepare.selectO {
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
      Prepare.selectIO(Input[String]) { name =>
        Select
          .from[Band]("b")
          .where { b => b.name === name }
          .returning { b => b }
      }

  }

  object MusicianInBandQueries extends TableQueries[MusicianInBand.Id, MusicianInBand]

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

  override val executable: Executable =
    Executable
      .withParser(Parser.unit)
      .withLayer {
        ZLayer.succeed(ConnectionFactory("jdbc:postgresql:postgres", "kalin", "psql-pass")) >+>
          JDBCConnection.connectionFactoryLayer.mapError(HError.SystemFailure("Unable to get db connection", _))
      }
      .withEffect {
        for {
          _ <- Logger.log.info("Starting...")

          _ <- PostgresMeta.schemaDiff(Tables(Musician.tableSchema, Band.tableSchema, MusicianInBand.tableSchema)).mapError(HError.SystemFailure("postgres-meta", _))

          // m1 = Sample.kalin
          // m2 = Sample.janine
          // m3 = Sample.bob
          // m4 = Sample.joy
          // _ <- MusicianQueries.insert(m1).mapError(HError.SystemFailure("insert m1", _))
          // _ <- MusicianQueries.insert(m2).mapError(HError.SystemFailure("insert m2", _))
          // _ <- MusicianQueries.insert(m3).mapError(HError.SystemFailure("insert m3", _))
          // _ <- MusicianQueries.insert(m4).mapError(HError.SystemFailure("insert m4", _))

          // b1 = Sample.theBest
          // b2 = Sample.anotherBand
          // _ <- BandQueries.insert(b1).mapError(HError.SystemFailure("insert b1", _))
          // _ <- BandQueries.insert(b2).mapError(HError.SystemFailure("insert b2", _))

          // mib1 = new MusicianInBand.Identity(id = MusicianInBand.Id.gen, musicianId = m1.id, bandId = b1.id, active = true)
          // mib2 = new MusicianInBand.Identity(id = MusicianInBand.Id.gen, musicianId = m2.id, bandId = b1.id, active = true)
          // mib3 = new MusicianInBand.Identity(id = MusicianInBand.Id.gen, musicianId = m1.id, bandId = b2.id, active = true)
          // mib4 = new MusicianInBand.Identity(id = MusicianInBand.Id.gen, musicianId = m4.id, bandId = b2.id, active = true)
          // _ <- MusicianInBandQueries.insert(mib1).mapError(HError.SystemFailure("insert mib1", _))
          // _ <- MusicianInBandQueries.insert(mib2).mapError(HError.SystemFailure("insert mib2", _))
          // _ <- MusicianInBandQueries.insert(mib3).mapError(HError.SystemFailure("insert mib3", _))
          // _ <- MusicianInBandQueries.insert(mib4).mapError(HError.SystemFailure("insert mib4", _))

          // kalin <- musicianByNames(("Kalin", "Rudnicki")).single.mapError(HError.InternalDefect("...", _))
          // janine <- musicianByNames(("Janine", "Rudnicki")).single.mapError(HError.InternalDefect("...", _))
          // joy <- musicianByNames(("Joy", "Rudnicki")).single.mapError(HError.InternalDefect("...", _))
          // bob <- musicianByNames(("Bob", "Rudnicki")).single.mapError(HError.InternalDefect("...", _))

          // theBest <- bandByName("The Best").single.mapError(HError.InternalDefect("...", _))
          // anotherBand <- bandByName("Another Band").single.mapError(HError.InternalDefect("...", _))

          // musicians <- MusicianQueries.selectAll().chunk.mapError(HError.InternalDefect("...", _))
          // _ <- Logger.log.info("")
          // _ <- ZIO.foreachDiscard(musicians)(Logger.log.info(_))
          // _ <- Logger.log.info("")
          // bands <- BandQueries.selectAll().chunk.mapError(HError.InternalDefect("...", _))
          // _ <- Logger.log.info(bands)
          // musicianInBands <- MusicianInBandQueries.selectAll().chunk.mapError(HError.InternalDefect("...", _))
          // _ <- Logger.log.info(musicianInBands)

          // pairs1 <- musicianAndBandNames().groupByLeft(_._1)(_._2).chunk.mapError(HError.InternalDefect("...", _))
          // _ <- Logger.log.info("")
          // _ <- ZIO.foreachDiscard(pairs1)(Logger.log.info(_))

          // _ <- Logger.log.info("")
          // m <- Musician.delete(UUID.fromString("845e6c5b-c202-4882-854f-887c53124f7b")).single.mapError(HError.InternalDefect("...", _))
          // _ <- Logger.log.info(m)

          // musicians <- MusicianQueries.selectAll().chunk.mapError(HError.InternalDefect("...", _))
          // _ <- Logger.log.info("")
          // _ <- ZIO.foreachDiscard(musicians)(Logger.log.info(_))
          // _ <- Logger.log.info("")
          // _ <- ZIO.scoped(MusicianQueries.musicianAndBandNames2.apply().stream.foreach(Logger.log.info(_)).mapError(HError.InternalDefect("...", _)))
          // _ <- Logger.log.info("")

          _ <- Logger.log.info("A")
          _ <- Logger.log.info("B", "context" -> "c")
          _ <- Logger.addContext("default-context" -> "dc") {
            Logger.log.info("C") *>
              Logger.log.info("D", "context" -> "c") *>
              Logger.log.info("E\nF", "context" -> "c2", "more-context" -> 5)
          }

        } yield ()
      }

}
