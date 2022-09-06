package harness.sql

import cats.~>
import java.time.*
import java.util.UUID
import shapeless3.deriving.*

object Tmp extends App {

  final case class Musician[F[_]](
      id: F[UUID],
      firstName: F[String],
      lastName: F[String],
      instrument: F[String],
      birthday: F[LocalDate],
  ) extends Table
  object Musician {

    val colInfo: Musician[Col] =
      Musician(
        id = Col.uuid("id"),
        firstName = Col.string("first_name"),
        lastName = Col.string("last_name"),
        instrument = Col.string("instrument"),
        birthday = Col.date("birthday"),
      )

    implicit val tableInfo: TableInfo[Musician] =
      TableInfo.derived[Musician]("musician")(
        colInfo,
      )

  }

  final case class Band[F[_]](
      id: F[UUID],
      name: F[String],
      formationDate: F[LocalDate],
  ) extends Table
  object Band {

    val colInfo: Band[Col] =
      Band(
        id = Col.uuid("id"),
        name = Col.string("name"),
        formationDate = Col.date("formation_date"),
      )

    implicit val tableInfo: TableInfo[Band] =
      TableInfo.derived[Band]("band")(
        colInfo,
      )

  }

  final case class MusicianInBand[F[_]](
      id: F[UUID],
      musicianId: F[UUID],
      bandId: F[UUID],
      active: F[Boolean],
  ) extends Table
  object MusicianInBand {

    val colInfo: MusicianInBand[Col] =
      MusicianInBand(
        id = Col.uuid("id"),
        musicianId = Col.uuid("musician_id"),
        bandId = Col.uuid("band_id"),
        active = Col.boolean("active"),
      )

    implicit val tableInfo: TableInfo[MusicianInBand] =
      TableInfo.derived[MusicianInBand]("musician_in_band")(
        colInfo,
      )

  }

  /*
    SELECT m.id, m.first_name, m.last_name, m.instrument, m.birthday, b.name
      FROM musician m
        JOIN musician_in_band mib ON mib.musician_id = m.id && !mib.active
        JOIN band b ON mib.band_id = b.id
      WHERE m.first_name = Kalin
   */

  Select
    .from[Musician]("m")
    .join[MusicianInBand]("mib")
    .on { (m, mib) => mib.musicianId === m.id && !mib.active }
    .join[Band]("b")
    .on { (_, mib, b) => mib.bandId === b.id }
    // .where { (m, _, _) => m.firstName === "Kalin" }
    .select { (m, _, b) => m ~ b.name }

  Select
    .from[Musician]("m")
    .leftJoin[MusicianInBand]("mib")
    .on { (m, mib) => mib.musicianId === m.id && !mib.active }
    .join[Band]("b")
    .on { (_, mib, b) => mib.bandId === b.id }
    .where { (_, mib, _) => mib.active }
    .select { (m, _, b) => m ~ b.name }

  println()

  println(Labelling[Musician[cats.Id]])
  println(Annotations[Col.Name, MusicianInBand[cats.Id]].apply().toList)
  println(K0.ProductGeneric[Musician[cats.Id]].toRepr(Musician(UUID.randomUUID, "F", "L", "I", LocalDate.now)))
  println()

}
