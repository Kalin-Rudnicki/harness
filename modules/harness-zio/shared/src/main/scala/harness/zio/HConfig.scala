package harness.zio

import cats.syntax.either.*
import cats.syntax.traverse.*
import harness.zio.error.{ConfigError, FSError}
import scala.annotation.tailrec
import zio.*
import zio.json.*
import zio.json.ast.Json

final case class HConfig(configJson: Json) { self =>

  def +(other: HConfig): HConfig = HConfig(self.configJson.merge(other.configJson))

  def read[A](jsonPath: String*)(implicit decoder: JsonDecoder[A]): IO[ConfigError.ReadError, A] = {
    @tailrec
    def loop(jsonPath: List[String], json: Json, rSeenJsonPath: List[String]): IO[ConfigError.ReadError, A] = {
      jsonPath match {
        case head :: tail =>
          json match {
            case Json.Obj(fields) =>
              fields.find(_._1 == head) match {
                case Some((_, json)) => loop(tail, json, head :: rSeenJsonPath)
                case None            => ZIO.fail(ConfigError.ReadError.ObjectMissingKey(rSeenJsonPath.reverse, head, json))
              }
            case _ => ZIO.fail(ConfigError.ReadError.ExpectedJsonObject(rSeenJsonPath.reverse, json))
          }
        case Nil =>
          // NOTE : `fromJsonAST` is not used, because ZIO json doesn't properly decode missing values to None with that function
          decoder.decodeJson(json.toString) match {
            case Right(value) => ZIO.succeed(value)
            case Left(error)  => ZIO.fail(ConfigError.ReadError.DecodingFailure(rSeenJsonPath.reverse, error, json))
          }
      }
    }

    loop(jsonPath.toList, configJson, Nil)
  }

  def readOpt[A](jsonPath: String*)(implicit decoder: JsonDecoder[A]): IO[ConfigError.ReadError.DecodingFailure, Option[A]] = {
    @tailrec
    def loop(jsonPath: List[String], json: Json, rSeenJsonPath: List[String]): IO[ConfigError.ReadError.DecodingFailure, Option[A]] = {
      jsonPath match {
        case head :: tail =>
          json match {
            case Json.Obj(fields) =>
              fields.find(_._1 == head) match {
                case Some((_, json)) => loop(tail, json, head :: rSeenJsonPath)
                case None            => ZIO.none
              }
            case _ => ZIO.none
          }
        case Nil =>
          // NOTE : `fromJsonAST` is not used, because ZIO json doesn't properly decode missing values to None with that function
          decoder.decodeJson(json.toString) match {
            case Right(value) => ZIO.some(value)
            case Left(error)  => ZIO.fail(ConfigError.ReadError.DecodingFailure(rSeenJsonPath.reverse, error, json))
          }
      }
    }

    loop(jsonPath.toList, configJson, Nil)
  }

}
object HConfig {

  val empty: HConfig =
    HConfig(Json.Obj())

  implicit val jsonCodec: JsonCodec[HConfig] =
    JsonCodec(Json.encoder, Json.decoder).transform(HConfig(_), _.configJson)

  // =====| Builders |=====

  def fromJson(json: Json): HConfig =
    HConfig(json)

  // TODO (KR) : make target optional
  def fromJsonString(json: String, target: ConfigError.ConfigTarget): IO[ConfigError.LoadError.FailedToDecode, HConfig] =
    ZIO.fromEither(json.fromJson[HConfig]).mapError(ConfigError.LoadError.FailedToDecode(target, _))

  def fromJarResource(path: String): IO[ConfigError.LoadError, HConfig] = {
    val target = ConfigError.ConfigTarget.JarResource(path)
    for {
      string <- JarUtils.findString(path).mapError(ConfigError.LoadError.Generic(target, _)).someOrFail(ConfigError.LoadError.ConfigTargetDNE(target))
      config <- HConfig.fromJsonString(string, target)
    } yield config
  }

  def fromPath(path: Path): IO[ConfigError.LoadError, HConfig] = {
    val target = ConfigError.ConfigTarget.File(path.show)
    for {
      fileExists <- path.exists.mapError(ConfigError.LoadError.Generic(target, _))
      _ <- ZIO.fail(ConfigError.LoadError.ConfigTargetDNE(target)).unless(fileExists)
      string <- path.readString.mapError(ConfigError.LoadError.Generic(target, _))
      config <- HConfig.fromJsonString(string, target)
    } yield config
  }

  def fromPathString(path: String): ZIO[FileSystem, ConfigError.LoadError, HConfig] =
    Path(path)
      .mapError(ConfigError.LoadError.Generic(ConfigError.ConfigTarget.File(path), _))
      .flatMap(HConfig.fromPath)

  def fromPaths(paths: List[Path]): IO[ConfigError.LoadError, HConfig] =
    ZIO.foldLeft(paths)(HConfig.empty) { (c, p) => HConfig.fromPath(p).map(c + _) }

  def fromPathStrings(paths: List[String]): ZIO[FileSystem, ConfigError.LoadError, HConfig] =
    ZIO.foldLeft(paths)(HConfig.empty) { (c, p) => HConfig.fromPathString(p).map(c + _) }

  def fromEnvVar(varName: String): IO[ConfigError.LoadError, HConfig] = {
    val target = ConfigError.ConfigTarget.EnvVar(varName)
    System
      .env(varName)
      .mapError(ConfigError.LoadError.Generic(target, _))
      .someOrFail(ConfigError.LoadError.ConfigTargetDNE(target))
      .flatMap(HConfig.fromJsonString(_, target))
  }

  def unsafeFromEncodable[A: JsonEncoder](a: A): HConfig =
    a.toJsonAST match {
      case Right(json) => HConfig.fromJson(json)
      case Left(error) => throw new RuntimeException(s"This should not happen... Unable to encode to json ast:\n$error\n$a")
    }

  def flatten(list: List[HConfig]): HConfig = list.foldLeft(empty)(_ + _)

  // =====| API |=====

  def read[A](jsonPath: String*)(implicit decoder: JsonDecoder[A]): ZIO[HConfig, ConfigError.ReadError, A] =
    ZIO.serviceWithZIO[HConfig](_.read[A](jsonPath*)(decoder))

  def readOpt[A](jsonPath: String*)(implicit decoder: JsonDecoder[A]): ZIO[HConfig, ConfigError.ReadError.DecodingFailure, Option[A]] =
    ZIO.serviceWithZIO[HConfig](_.readOpt[A](jsonPath*)(decoder))

  def readLayer[A: Tag](jsonPath: String*)(implicit decoder: JsonDecoder[A]): ZLayer[HConfig, ConfigError.ReadError, A] =
    ZLayer.fromZIO { HConfig.read[A](jsonPath*)(decoder) }

  // =====| Layers |=====

  sealed abstract class LayerBuilder[R](make: HConfig => URIO[R, HConfig]) {

    final def json(json: Json): URLayer[R, HConfig] =
      ZLayer.fromZIO { make(HConfig(json)) }

    final def jsonString(json: String): ZLayer[R, ConfigError.LoadError, HConfig] =
      ZLayer.fromZIO { HConfig.fromJsonString(json, ConfigError.ConfigTarget.RawString).flatMap(make) }

    final def jarResource(path: String): ZLayer[R, ConfigError.LoadError, HConfig] =
      ZLayer.fromZIO { HConfig.fromJarResource(path).flatMap(make) }

    final def path(path: Path): ZLayer[R, ConfigError.LoadError, HConfig] =
      ZLayer.fromZIO { HConfig.fromPath(path).flatMap(make) }

    final def paths(paths: List[Path]): ZLayer[R, ConfigError.LoadError, HConfig] =
      ZLayer.fromZIO { HConfig.fromPaths(paths).flatMap(make) }

    final def pathString(path: String): ZLayer[FileSystem & R, ConfigError.LoadError, HConfig] =
      ZLayer.fromZIO { HConfig.fromPathString(path).flatMap(make) }

    final def pathStrings(paths: List[String]): ZLayer[FileSystem & R, ConfigError.LoadError, HConfig] =
      ZLayer.fromZIO { HConfig.fromPathStrings(paths).flatMap(make) }

  }

  object layer extends LayerBuilder[Any](ZIO.succeed(_)) {

    val empty: ULayer[HConfig] =
      ZLayer.succeed(HConfig.empty)

    object prepend extends LayerBuilder[HConfig](cfg => ZIO.serviceWith[HConfig](cfg + _))
    object append extends LayerBuilder[HConfig](cfg => ZIO.serviceWith[HConfig](_ + cfg))

  }

  // =====| KeyedConfig |=====

  final case class KeyedConfig(
      key: String,
      config: Json,
  )
  object KeyedConfig {

    implicit val jsonCodec: JsonCodec[KeyedConfig] = DeriveJsonCodec.gen

    def makeDecoder[A](decoders: KeyedConfigDecoder[A]*): JsonDecoder[A] =
      KeyedConfig.jsonCodec.decoder.mapOrFail { keyedConfig =>
        decoders.find(_.key == keyedConfig.key) match {
          case Some(decoder) => decoder.decoder.decodeJson(keyedConfig.config.toString).leftMap { err => s".config($err)" }
          case None          => s".key(Invalid key '${keyedConfig.key}', expected one of: ${decoders.map(_.key).mkString(", ")})".asLeft
        }
      }

    def makeMapDecoder[A](decoders: KeyedConfigDecoder[A]*): JsonDecoder[List[A]] =
      JsonDecoder.map[String, Option[Json]].mapOrFail {
        _.toList.collect { case (key, Some(json)) => (key, json) }.map(KeyedConfig.apply).traverse { keyedConfig =>
          decoders.find(_.key == keyedConfig.key) match {
            case Some(decoder) => decoder.decoder.decodeJson(keyedConfig.config.toString).leftMap { err => s".${keyedConfig.key}($err)" }
            case None          => s"Invalid key '${keyedConfig.key}', expected one of: ${decoders.map(_.key).mkString(", ")}".asLeft
          }
        }
      }

  }

  final case class KeyedConfigDecoder[A](
      key: String,
      decoder: JsonDecoder[A],
  )
  object KeyedConfigDecoder {

    def makeEither[Cfg, A](key: String)(map: Cfg => Either[String, A])(implicit rawDecoder: JsonDecoder[Cfg]): KeyedConfigDecoder[A] =
      KeyedConfigDecoder(
        key,
        rawDecoder.mapOrFail(map),
      )

    def make[Cfg, A](key: String)(map: Cfg => A)(implicit rawDecoder: JsonDecoder[Cfg]): KeyedConfigDecoder[A] =
      KeyedConfigDecoder(
        key,
        rawDecoder.map(map),
      )

  }

}
