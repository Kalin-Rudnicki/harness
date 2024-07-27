package harness.schema

import harness.zio.*
import java.util.UUID

final case class SchemaRef(tag: HTag[?], id: UUID) {

  private[schema] def withId(id: UUID): SchemaRef = copy(id = id)

  private[schema] def optReplaceId(oldId: UUID, newId: UUID): SchemaRef =
    if (id == oldId) withId(newId)
    else this

}
object SchemaRef {

  def gen(tag: HTag[?]): SchemaRef =
    SchemaRef(tag, new UUID(scala.util.Random.nextLong, scala.util.Random.nextLong))

  implicit val ordering: Ordering[SchemaRef] = Ordering.by(_.tag)

}
