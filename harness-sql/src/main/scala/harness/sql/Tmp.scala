package harness.sql

import cats.~>
import cats.syntax.option.*
import harness.cli.*
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
  ) extends Table
  object Musician extends Table.Companion[Musician] {

    override implicit val tableInfo: TableInfo[Musician] =
      TableInfo.derived[Musician]("musician")(
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
  ) extends Table
  object Band extends Table.Companion[Band] {

    override implicit val tableInfo: TableInfo[Band] =
      TableInfo.derived[Band]("band")(
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
  ) extends Table
  object MusicianInBand extends Table.Companion[MusicianInBand] {

    override implicit val tableInfo: TableInfo[MusicianInBand] =
      TableInfo.derived[MusicianInBand]("musician_in_band")(
        MusicianInBand[Col](
          id = Col.uuid("id"),
          musicianId = Col.uuid("musician_id"),
          bandId = Col.uuid("band_id"),
          active = Col.boolean("active"),
        ),
      )

  }

  // =====|  |=====

  val insertMusician: InsertQueryI[Musician.Id] =
    Prepare.insertO {
      Insert.into[Musician]
    }

  val musicians: SelectQueryO[Musician.Id] =
    Prepare.selectO {
      Select
        .from[Musician]("m")
        .returning { m => m }
    }

  val musicianByNames: SelectQueryIO[(String, String), Musician.Id] =
    Prepare.selectIO(Input[String] ~ Input[String]) { (first, last) =>
      Select
        .from[Musician]("m")
        .where { m => m.lastName === last && m.firstName === first }
        .returning { m => m }
    }

  val musicianAndBandNames: SelectQueryO[(Musician.Id, String)] =
    Prepare.selectO {
      Select
        .from[Musician]("m")
        .join[MusicianInBand]("mib")
        .on { (m, mib) => mib.musicianId === m.id && !mib.active }
        .join[Band]("b")
        .on { (_, mib, b) => mib.bandId === b.id }
        .where { (_, mib, _) => mib.active }
        .returning { (m, _, b) => m ~ b.name }
    }

  val m1: Musician.Id =
    new Musician.Id(
      id = UUID.randomUUID,
      firstName = "Janine",
      lastName = "Rudnicki",
      instrument = "Homework",
      birthday = LocalDate.of(2001, 2, 19),
      favoriteNumber = None,
    )

  override val executable: Executable =
    Executable
      .withParser(Parser.unit)
      .withLayer { ZLayer.succeed(ConnectionFactory("jdbc:postgresql:postgres", "kalin", "psql-pass")) }
      .withEffect {
        for {
          _ <- Logger.log.info("Starting...")
          // i <- insertMusician(m1)
          // _ <- Logger.log.info(i)
          ms <- musicians().chunk
          _ <- Logger.log.info(ms)
          m <- musicianByNames(("Kalin", "Rudnicki")).single
          _ <- Logger.log.info(m)
        } yield ()
      }

}
