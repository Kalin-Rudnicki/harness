package harness.zio.error

import harness.core.*
import harness.zio.json.throwableJsonCodec
import zio.json.*

sealed trait ConfigError extends Throwable {

  override final def getMessage: String =
    this match {
      case loadError: ConfigError.LoadError =>
        loadError match {
          case ConfigError.LoadError.ConfigTargetDNE(target) =>
            s"Config target does not exist: $target"
          case ConfigError.LoadError.FailedToDecode(target, error) =>
            s"Failed to decode config target $target: $error"
          case ConfigError.LoadError.Generic(target, cause) =>
            s"Generic error loading config target $target: ${cause.safeGetMessage}"
        }
      case readError: ConfigError.ReadError =>
        val baseMessage: String =
          readError match {
            case ConfigError.ReadError.ObjectMissingKey(_, key, presentKeys) =>
              s"Config object is missing key '$key' at path ${readError.showPath}, present keys: ${presentKeys.toList.sorted.mkString("[", ", ", "]")}"
            case ConfigError.ReadError.ExpectedJsonObject(_, actual) =>
              s"Expected json object at path ${readError.showPath}, actual: $actual"
            case ConfigError.ReadError.DecodingFailure(_, message) =>
              s"Failed to decode config object at path ${readError.showPath}: $message"
          }
        s"Config error @ ${readError.showPath}:\n$baseMessage\nConfig json @ ${readError.showPath}"
    }

}
object ConfigError {

  sealed trait LoadError extends ConfigError
  object LoadError {
    final case class ConfigTargetDNE(target: ConfigTarget) extends ConfigError.LoadError
    final case class FailedToDecode(target: ConfigTarget, error: String) extends ConfigError.LoadError
    final case class Generic(target: ConfigTarget, cause: Throwable) extends ConfigError.LoadError
  }

  sealed trait ReadError extends ConfigError {
    val path: List[String]
    final lazy val showPath: String = path match {
      case Nil => "<root>"
      case _   => path.mkString(".")
    }
  }
  object ReadError {
    final case class ObjectMissingKey(path: List[String], key: String, presentKeys: Set[String]) extends ConfigError.ReadError
    final case class ExpectedJsonObject(path: List[String], actual: String) extends ConfigError.ReadError
    final case class DecodingFailure(path: List[String], message: String) extends ConfigError.ReadError
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
