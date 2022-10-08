package harness.sql

import cats.~>
import cats.syntax.option.*
import harness.cli.*
import harness.core.*
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

  val m1: Musician.Identity =
    new Musician.Identity(
      id = Musician.Id.gen,
      firstName = "Joy",
      lastName = "Rudnicki",
      instrument = "Piano",
      birthday = LocalDate.of(1967, 12, 20),
      favoriteNumber = 30.some,
    )

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

          // kalin <- musicianByNames(("Kalin", "Rudnicki")).single.mapError(HError.InternalDefect("...", _))
          // janine <- musicianByNames(("Janine", "Rudnicki")).single.mapError(HError.InternalDefect("...", _))
          // joy <- musicianByNames(("Joy", "Rudnicki")).single.mapError(HError.InternalDefect("...", _))
          // bob <- musicianByNames(("Bob", "Rudnicki")).single.mapError(HError.InternalDefect("...", _))

          // theBest <- bandByName("The Best").single.mapError(HError.InternalDefect("...", _))
          // anotherBand <- bandByName("Another Band").single.mapError(HError.InternalDefect("...", _))

          musicians <- MusicianQueries.selectAll().chunk.mapError(HError.InternalDefect("...", _))
          _ <- Logger.log.info("")
          _ <- ZIO.foreachDiscard(musicians)(Logger.log.info(_))
          _ <- Logger.log.info("")
          bands <- BandQueries.selectAll().chunk.mapError(HError.InternalDefect("...", _))
          _ <- Logger.log.info(bands)
          musicianInBands <- MusicianInBandQueries.selectAll().chunk.mapError(HError.InternalDefect("...", _))
          _ <- Logger.log.info(musicianInBands)

          // pairs1 <- musicianAndBandNames().groupByLeft(_._1)(_._2).chunk.mapError(HError.InternalDefect("...", _))
          // _ <- Logger.log.info("")
          // _ <- ZIO.foreachDiscard(pairs1)(Logger.log.info(_))

          // _ <- Logger.log.info("")
          // m <- Musician.delete(UUID.fromString("845e6c5b-c202-4882-854f-887c53124f7b")).single.mapError(HError.InternalDefect("...", _))
          // _ <- Logger.log.info(m)

          musicians <- MusicianQueries.selectAll().chunk.mapError(HError.InternalDefect("...", _))
          _ <- Logger.log.info("")
          _ <- ZIO.foreachDiscard(musicians)(Logger.log.info(_))
          _ <- Logger.log.info("")

        } yield ()
      }

}
