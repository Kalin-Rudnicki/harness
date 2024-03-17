package harness.zio.error

import harness.core.*
import harness.zio.ZIOJsonInstances.throwableJsonCodec
import zio.json.*
import zio.json.ast.Json

sealed trait ConfigError extends Throwable {

  override final def getMessage: String = {
    def showPath(path: List[String]): String =
      path match {
        case Nil => s"`root`"
        case _   => path.mkString(".")
      }

    this match {
      case error: ConfigError.LoadError =>
        error match {
          case ConfigError.LoadError.ConfigTargetDNE(target) =>
            s"Config target does not exist: $target"
          case ConfigError.LoadError.FailedToDecode(target, error) =>
            s"Failed to decode config target $target: $error"
          case ConfigError.LoadError.Generic(target, cause) =>
            s"Generic error loading config target $target: ${cause.safeGetMessage}"
        }
      case error: ConfigError.ReadError =>
        error match {
          case ConfigError.ReadError.ObjectMissingKey(path, key, json) =>
            s"Config object is missing key '$key' at path ${showPath(path)}\nJson: ${json.toJson}"
          case ConfigError.ReadError.ExpectedJsonObject(path, json) =>
            s"Config object at path ${showPath(path)} expected object\nJson: ${json.toJson}"
          case ConfigError.ReadError.DecodingFailure(path, message, json) =>
            s"Failed to decode config object at path ${showPath(path)}: $message\nJson: ${json.toJson}"
        }
    }
  }

}
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
    final case class ExpectedJsonObject(path: List[String], json: Json) extends ConfigError.ReadError
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
