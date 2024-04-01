package harness.schema

import zio.json.JsonCodec

implicit def schemaToJsonCodec[A](implicit schema: JsonSchema[A]): JsonCodec[A] = schema.codec
