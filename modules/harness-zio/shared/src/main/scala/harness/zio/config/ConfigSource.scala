package harness.zio.config

import harness.cli.*
import harness.core.*
import harness.zio.*
import harness.zio.error.ConfigError
import java.io.ByteArrayInputStream
import zio.*
import zio.json.*
import zio.json.ast.Json

@jsonDiscriminator("source")
sealed trait ConfigSource derives JsonCodec {

  private[zio] def configTarget: ConfigError.ConfigTarget = this match
    case ConfigSource.File(path)        => ConfigError.ConfigTarget.File(path)
    case ConfigSource.JarResource(path) => ConfigError.ConfigTarget.JarResource(path)
    case ConfigSource.EnvVar(varName)   => ConfigError.ConfigTarget.EnvVar(varName)
    case ConfigSource.Raw(_)            => ConfigError.ConfigTarget.RawString

  private def attemptRead: Task[Option[String]] = this match
    case ConfigSource.File(path)        => Path(path).flatMap { f => f.readString.whenZIO(f.exists) }
    case ConfigSource.JarResource(path) => JarUtils.findString(path)
    case ConfigSource.EnvVar(varName)   => System.env(varName)
    case ConfigSource.Raw(raw)          => ZIO.some(raw)

  final def read: IO[ConfigError.LoadError, String] =
    attemptRead
      .mapError(ConfigError.LoadError.Generic(configTarget, _))
      .someOrFail(ConfigError.LoadError.ConfigTargetDNE(configTarget))

  private def attemptInputStream: RIO[Scope, Option[java.io.InputStream]] = this match
    case ConfigSource.File(path)        => Path(path).flatMap { f => f.inputStream.whenZIO(f.exists) }
    case ConfigSource.JarResource(path) => JarUtils.findInputStream(path)
    case ConfigSource.EnvVar(varName)   => System.env(varName).map { _.map { str => new ByteArrayInputStream(str.getBytes) } }
    case ConfigSource.Raw(raw)          => ZIO.some(new ByteArrayInputStream(raw.getBytes))

  final def readInputStream: ZIO[Scope, ConfigError.LoadError, java.io.InputStream] =
    attemptInputStream
      .mapError(ConfigError.LoadError.Generic(configTarget, _))
      .someOrFail(ConfigError.LoadError.ConfigTargetDNE(configTarget))

}
object ConfigSource {

  final case class File(path: String) extends ConfigSource
  final case class JarResource(path: String) extends ConfigSource
  final case class EnvVar(varName: String) extends ConfigSource
  final case class Raw(raw: String) extends ConfigSource

  val parser: Params[ConfigSource] = {
    val jsonPathValue: Values[List[String]] =
      Values.value[String]("json-path", hints = List("Json path to nest the following json in", "ex: a.b.c")).map(_.split('.').toList.map(_.trim).filter(_.nonEmpty))
    val jsonValue: Values[Json] =
      Values.value[String]("json", hints = List("Will attempt to parse to json AST", "if that fails, it will be treated as a string")).map { str => str.fromJson[Json].getOrElse(Json.Str(str)) }

    Params.firstOf(
      Params.value[String]("file", 'f', hints = List("Read config from file")).map(File(_)),
      Params.value[String]("jar", 'j', hints = List("Read config from jar resource")).map(JarResource(_)),
      Params.value[String]("env", 'e', hints = List("Read config from env var")).map(EnvVar(_)),
      Params
        .valueWith("json", 'J', hints = List("Specify config json directly on the command line"))(
          jsonValue <||
            (jsonPathValue ^>> jsonValue).map { case (path, json) => path.foldRight(json) { (p, j) => Json.Obj(p -> j) } },
        )
        .map { json => Raw(json.toString) },
      Params.value[String]("raw", 'r', hints = List("Specify raw config directly on the command line")).map(Raw(_)),
    )
  }

}

final class EncodedConfigSource[A](source: ConfigSource, decoder: StringDecoder[A]) {

  def read: IO[ConfigError.LoadError, A] =
    source.read.flatMap {
      decoder.decode(_) match {
        case Right(value) => ZIO.succeed(value)
        case Left(error)  => ZIO.fail(ConfigError.LoadError.FailedToDecode(source.configTarget, error))
      }
    }

}
object EncodedConfigSource {

  implicit def jsonDecoder[A: StringDecoder]: JsonDecoder[EncodedConfigSource[A]] =
    JsonDecoder[ConfigSource].map(EncodedConfigSource(_, StringDecoder[A]))

  def parser[A: StringDecoder]: Params[EncodedConfigSource[A]] =
    ConfigSource.parser.map(EncodedConfigSource(_, StringDecoder[A]))

}
