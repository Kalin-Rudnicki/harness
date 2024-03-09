package harness.pk

import harness.core.*
import java.util.UUID
import scala.util.matching.Regex
import zio.*
import zio.json.*

trait TableKey { self =>

  final case class Id(toUUID: UUID) {
    override def toString: String =
      this.getClass.getName match {
        case Id.idRegex(className) => s"$className($toUUID)"
        case _                     => toUUID.toString
      }
  }
  object Id {

    implicit val stringEncoder: StringEncoder[Id] = StringEncoder.uuid.imap[Id](_.toUUID)
    implicit val stringDecoder: StringDecoder[Id] = StringDecoder.uuid.map(Id(_))
    implicit val jsonCodec: JsonCodec[Id] = JsonCodec.uuid.transform(Id(_), _.toUUID)
    implicit val iMap: IMap[UUID, Id] = IMap.make[UUID](Id(_))(_.toUUID)

    private val idRegex: Regex = "([^.]+)\\.Id$".r

  }

  inline def apply(uuid: UUID): Id = Id(uuid)
  def gen: Id = Id(UUID.randomUUID())
  def genZio: UIO[Id] = Random.nextUUID.map(Id(_))

}
