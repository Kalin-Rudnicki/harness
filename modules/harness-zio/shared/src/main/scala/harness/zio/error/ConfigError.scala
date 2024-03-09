package harness.zio.error

import harness.zio.JsonShowable
import harness.zio.ZIOJsonInstances.throwableJsonCodec
import zio.json.*
import zio.json.ast.Json

sealed trait ConfigError extends Throwable with JsonShowable[ConfigError]
object ConfigError {

  sealed trait LoadError extends ConfigError
  object LoadError {
    final case class ConfigTargetDNE(target: ConfigTarget) extends ConfigError.LoadError
    final case class FailedToDecode(target: ConfigTarget, error: String) extends ConfigError.LoadError
    final case class Generic(target: ConfigTarget, cause: Throwable) extends ConfigError.LoadError
  }

  sealed trait ReadError extends ConfigError
  object ReadError {
    final case class ObjectMissingKey(path: List[String], key: String, json: Json) extends ConfigError.ReadError
    final case class UnexpectedNonObject(path: List[String], json: Json) extends ConfigError.ReadError
    final case class DecodingFailure(path: List[String], message: String, json: Json) extends ConfigError.ReadError
  }

  sealed trait ConfigTarget
  object ConfigTarget {
    final case class File(path: String) extends ConfigTarget
    final case class JarResource(path: String) extends ConfigTarget
    final case class EnvVar(varName: String) extends ConfigTarget
    case object RawString extends ConfigTarget

    implicit val jsonCodec: JsonCodec[ConfigTarget] = DeriveJsonCodec.gen
  }

  implicit val jsonCodec: JsonCodec[ConfigError] = DeriveJsonCodec.gen

}
