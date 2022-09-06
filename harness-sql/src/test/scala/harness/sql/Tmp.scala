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
      id: F[UUID],
      firstName: F[String],
      lastName: F[String],
      instrument: F[String],
      birthday: F[LocalDate],
      favoriteNumber: F[Option[Int]],
  ) extends Table.WithId[F]
  object Musician extends Table.Companion.WithId[Musician] {

    override implicit val tableSchema: TableSchema[Musician] =
      TableSchema.derived[Musician]("musician")(
        Musician[Col](
          id = Col.uuid("id"),
          firstName = Col.string("first_name"),
          lastName = Col.string("last_name"),
          instrument = Col.string("instrument"),
          birthday = Col.date("birthday"),
          favoriteNumber = Col.int("favorite_number").imap(_ - 10)(_ + 10).optional,
        ),
      )

  }

  final case class Band[F[_]](
      id: F[UUID],
      name: F[String],
      formationDate: F[LocalDate],
  ) extends Table.WithId[F]
  object Band extends Table.Companion.WithId[Band] {

    override implicit val tableSchema: TableSchema[Band] =
      TableSchema.derived[Band]("band")(
        Band[Col](
          id = Col.uuid("id"),
          name = Col.string("name"),
          formationDate = Col.date("formation_date"),
        ),
      )

  }

  final case class MusicianInBand[F[_]](
      id: F[UUID],
      musicianId: F[UUID],
      bandId: F[UUID],
      active: F[Boolean],
  ) extends Table.WithId[F]
  object MusicianInBand extends Table.Companion.WithId[MusicianInBand] {

    override implicit val tableSchema: TableSchema[MusicianInBand] =
      TableSchema.derived[MusicianInBand]("musician_in_band")(
        MusicianInBand[Col](
          id = Col.uuid("id"),
          musicianId = Col.uuid("musician_id"),
          bandId = Col.uuid("band_id"),
          active = Col.boolean("active"),
        ),
      )

  }

  // =====|  |=====

  val musicians: QueryO[Musician.Id] =
    Prepare.selectO {
      Select
        .from[Musician]("m")
        .returning { m => m }
    }

  val musicianById: QueryIO[UUID, Musician.Id] =
    Prepare.selectIO(Input[UUID]) { id =>
      Select
        .from[Musician]("m")
        .where { m => m.id === id }
        .returning { m => m }
    }

  val musicianByNames: QueryIO[(String, String), Musician.Id] =
    Prepare.selectIO(Input[String] ~ Input[String]) { (first, last) =>
      Select
        .from[Musician]("m")
        .where { m => m.lastName === last && m.firstName === first }
        .returning { m => m }
    }

  val musicianSetFavoriteNumber: QueryIO[(UUID, Option[Int]), Musician.Id] =
    Prepare.updateIO(Input[UUID] ~ Input[Option[Int]]) { (id, fn) =>
      Update[Musician]("m")
        .where { m => m.id === id }
        .set { m => m.favoriteNumber := fn }
        .returning { m => m }
    }

  val musicianDelete: QueryIO[UUID, Musician.Id] =
    Prepare.deleteIO(Input[UUID]) { id =>
      Delete
        .from[Musician]("m")
        .where { m => m.id === id }
        .returning { m => m }
    }

  val bandByName: QueryIO[String, Band.Id] =
    Prepare.selectIO(Input[String]) { name =>
      Select
        .from[Band]("b")
        .where { b => b.name === name }
        .returning { b => b }
    }

  val musicianWithFavNumGreaterThan: QueryIO[Int, Musician.Id] =
    Prepare.selectIO(Input[Int]) { num =>
      Select
        .from[Musician]("m")
        .where { m => m.favoriteNumber > num }
        .returning { m => m }
    }

  val musicianAndBandNames: QueryO[(Musician.Id, Option[String])] =
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

  val m1: Musician.Id =
    new Musician.Id(
      id = UUID.randomUUID,
      firstName = "Joy",
      lastName = "Rudnicki",
      instrument = "Piano",
      birthday = LocalDate.of(1967, 12, 20),
      favoriteNumber = 30.some,
    )

  override val executable: Executable =
    Executable
      .withParser(Parser.unit)
      .withLayer { ZLayer.succeed(ConnectionFactory("jdbc:postgresql:postgres", "kalin", "psql-pass")) }
      .withEffect {
        for {
          _ <- Logger.log.info("Starting...")

          // kalin <- musicianByNames(("Kalin", "Rudnicki")).single.mapError(HError.InternalDefect("...", _))
          // janine <- musicianByNames(("Janine", "Rudnicki")).single.mapError(HError.InternalDefect("...", _))
          // joy <- musicianByNames(("Joy", "Rudnicki")).single.mapError(HError.InternalDefect("...", _))
          // bob <- musicianByNames(("Bob", "Rudnicki")).single.mapError(HError.InternalDefect("...", _))

          // theBest <- bandByName("The Best").single.mapError(HError.InternalDefect("...", _))
          // anotherBand <- bandByName("Another Band").single.mapError(HError.InternalDefect("...", _))

          musicians <- Musician.selectAll().chunk.mapError(HError.InternalDefect("...", _))
          _ <- Logger.log.info("")
          _ <- ZIO.foreachDiscard(musicians)(Logger.log.info(_))
          _ <- Logger.log.info("")
          bands <- Band.selectAll().chunk.mapError(HError.InternalDefect("...", _))
          _ <- Logger.log.info(bands)
          musicianInBands <- MusicianInBand.selectAll().chunk.mapError(HError.InternalDefect("...", _))
          _ <- Logger.log.info(musicianInBands)

          // pairs1 <- musicianAndBandNames().groupByLeft(_._1)(_._2).chunk.mapError(HError.InternalDefect("...", _))
          // _ <- Logger.log.info("")
          // _ <- ZIO.foreachDiscard(pairs1)(Logger.log.info(_))

          _ <- Logger.log.info("")
          m <- Musician.delete(UUID.fromString("845e6c5b-c202-4882-854f-887c53124f7b")).single.mapError(HError.InternalDefect("...", _))
          _ <- Logger.log.info(m)

          musicians <- Musician.selectAll().chunk.mapError(HError.InternalDefect("...", _))
          _ <- Logger.log.info("")
          _ <- ZIO.foreachDiscard(musicians)(Logger.log.info(_))
          _ <- Logger.log.info("")

        } yield ()
      }

}
