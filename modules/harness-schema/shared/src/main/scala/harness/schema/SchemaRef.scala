package harness.schema

import java.util.UUID
import zio.Tag

final case class SchemaRef(tag: Tag[?], id: UUID)
object SchemaRef {

  def gen(tag: Tag[?]): SchemaRef =
    SchemaRef(tag, new UUID(scala.util.Random.nextLong, scala.util.Random.nextLong))

}
