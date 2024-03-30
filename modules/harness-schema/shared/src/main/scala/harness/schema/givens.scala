package harness.schema

import zio.json.JsonCodec

given schemaToJsonCodec[A](using schema: JsonSchema[A]): JsonCodec[A] = schema.codec
