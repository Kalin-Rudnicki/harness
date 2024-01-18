package harness.pk

import harness.core.*
import java.util.UUID
import zio.*
import zio.json.*

trait StringId { self =>

  final case class Id(value: String) {
    override def toString: String = value
  }
  object Id {

    implicit val stringEncoder: StringEncoder[Id] = StringEncoder.string.imap[Id](_.value)
    implicit val stringDecoder: StringDecoder[Id] = StringDecoder.string.map(Id(_))
    implicit val jsonCodec: JsonCodec[Id] = JsonCodec.string.transform(Id(_), _.value)
    implicit val iMap: IMap[String, Id] = IMap.make[String](Id(_))(_.value)

  }

  inline def apply(value: String): Id = Id(value)
  def gen: Id = Id(UUID.randomUUID().toString)
  def genZio: UIO[Id] = Random.nextUUID.map(uuid => Id(uuid.toString))

}
