package harness.schema

import java.util.UUID
import zio.Tag

final case class SchemaRef(tag: Tag[?], id: UUID)
