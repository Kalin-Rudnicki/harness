package harness.pk

import harness.core.*
import harness.schema.*
import harness.zio.TypeOps
import java.util.UUID
import zio.*

trait TableKey { self =>

  final case class Id(toUUID: UUID) {
    override def toString: String = s"${Id.tag.typeName.prefixNoneNoGenerics}($toUUID)"
  }
  object Id {

    implicit val stringEncoder: StringEncoder[Id] = StringEncoder.uuid.imap[Id](_.toUUID)
    implicit val stringDecoder: StringDecoder[Id] = StringDecoder.uuid.map(Id(_))
    implicit val iMap: IMap[UUID, Id] = IMap.make[UUID](Id(_))(_.toUUID)

    implicit val tag: Tag[Id] = TypeOps.tagFromClass(self.getClass)

    implicit val rawSchema: RawSchema[Id] = RawSchema.encodedStringSchema
    implicit val jsonSchema: JsonSchema[Id] = JsonSchema.encodedStringSchema

  }

  inline def apply(uuid: UUID): Id = Id(uuid)
  def gen: Id = Id(UUID.randomUUID())
  def genZio: UIO[Id] = Random.nextUUID.map(Id(_))

}
