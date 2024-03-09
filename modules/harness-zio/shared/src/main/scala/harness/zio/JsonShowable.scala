package harness.zio

import zio.json.*

trait JsonShowable[T](implicit jsonEncoder: JsonEncoder[T]) { self: T =>
  final def showJson: String = self.toJson(using jsonEncoder)
  final def showJsonPretty: String = self.toJsonPretty(using jsonEncoder)
}
