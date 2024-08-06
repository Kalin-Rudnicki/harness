package harness.zio.json

import zio.json.*
import zio.json.ast.Json

extension [A](self: A) {
  def safeToJsonAST(implicit encoder: JsonEncoder[A]): Json = {
    val encoded = self.toJson
    Json.decoder.decodeJson(encoded).getOrElse(Json.Str(encoded))
  }
}

extension (self: Json) {
  def jsonToString: String = self match
    case Json.Str(value) => value
    case _               => self.toJson
  def jsonToStringPretty: String = self match
    case Json.Str(value) => value
    case _               => self.toJsonPretty
}
