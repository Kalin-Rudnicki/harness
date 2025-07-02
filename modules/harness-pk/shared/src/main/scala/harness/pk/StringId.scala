package harness.pk

import harness.core.*
import harness.schema.*
import java.util.UUID
import zio.*

trait StringId { self =>

  final case class Id(value: String) {
    override def toString: String = s"${getClass.getName}($value)"
  }
  object Id {

    implicit val stringEncoder: StringEncoder[Id] = StringEncoder.string.imap[Id](_.value)
    implicit val stringDecoder: StringDecoder[Id] = StringDecoder.string.map(Id(_))
    implicit val iMap: IMap[String, Id] = IMap.make[String](Id(_))(_.value)

    implicit val rawSchema: RawSchema[Id] = RawSchema.encodedStringSchema
    implicit val jsonSchema: JsonSchema[Id] = JsonSchema.encodedStringSchema

  }

  inline def apply(value: String): Id = Id(value)
  def gen: Id = Id(UUID.randomUUID().toString)
  def genZio: UIO[Id] = Random.nextUUID.map(uuid => Id(uuid.toString))

}
