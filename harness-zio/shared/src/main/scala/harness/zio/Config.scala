package harness.zio

import cats.data.NonEmptyList
import cats.syntax.either.*
import cats.syntax.traverse.*
import harness.core.*
import scala.annotation.tailrec
import zio.*
import zio.json.*
import zio.json.ast.Json

final case class Config(configJson: Json) { self =>

  def +(other: Config): Config = Config(self.configJson.merge(other.configJson))

  def read[A](jsonPath: String*)(implicit decoder: JsonDecoder[A]): HTask[A] = {
    @tailrec
    def loop(jsonPath: List[String], json: Json, rSeenJsonPath: List[String]): HTask[A] = {
      inline def fail(msg: String): HTask[Nothing] =
        ZIO.fail(HError.InternalDefect(s"Unable to decode at path ${rSeenJsonPath.reverse.mkString("[", ".", "]")}: $msg\njson:\n${json.toJsonPretty}"))

      jsonPath match {
        case head :: tail =>
          json match {
            case Json.Obj(fields) =>
              fields.find(_._1 == head) match {
                case Some((_, json)) => loop(tail, json, head :: rSeenJsonPath)
                case None            => fail(s"Object is missing key '$head'")
              }
            case _ => fail(s"Expected object with key '$head', but found $json")
          }
        case Nil =>
          // NOTE : `fromJsonAST` is not used, because ZIO json doesn't properly decode missing values to None with that function
          decoder.decodeJson(json.toString) match {
            case Right(value) => ZIO.succeed(value)
            case Left(error)  => fail(error)
          }
      }
    }

    loop(jsonPath.toList, configJson, Nil)
  }

  def readOpt[A](jsonPath: String*)(implicit decoder: JsonDecoder[A]): HTask[Option[A]] = {
    @tailrec
    def loop(jsonPath: List[String], json: Json, rSeenJsonPath: List[String]): HTask[Option[A]] = {
      inline def fail(msg: String): HTask[Nothing] =
        ZIO.fail(HError.InternalDefect(s"Unable to decode at path ${rSeenJsonPath.reverse.mkString("[", ".", "]")}: $msg\njson:\n${json.toJsonPretty}"))

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
            case Left(error)  => fail(error)
          }
      }
    }

    loop(jsonPath.toList, configJson, Nil)
  }

}
object Config {

  val empty: Config =
    Config(Json.Obj())

  def fromJson(json: Json): Config =
    Config(json)

  def fromJsonString(json: String): HTask[Config] =
    json.fromJson[Json] match {
      case Right(json) => ZIO.succeed(Config(json))
      case Left(error) => ZIO.fail(HError.InternalDefect(error))
    }

  def flatten(list: List[Config]): Config = list.foldLeft(empty)(_ + _)

  implicit val jsonCodec: JsonCodec[Config] =
    JsonCodec(Json.encoder, Json.decoder).transform(Config(_), _.configJson)

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
      JsonDecoder.map[String, Json].mapOrFail {
        _.toList.map(KeyedConfig.apply).traverse { keyedConfig =>
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

    def make[Cfg, A](key: String)(map: Cfg => Either[String, A])(implicit rawDecoder: JsonDecoder[Cfg]): KeyedConfigDecoder[A] =
      KeyedConfigDecoder(
        key,
        rawDecoder.mapOrFail(map),
      )

  }

  // =====| API |=====

  def read[A](jsonPath: String*)(implicit decoder: JsonDecoder[A]): HRIO[Config, A] =
    ZIO.serviceWithZIO[Config](_.read[A](jsonPath*)(decoder))

  def readOpt[A](jsonPath: String*)(implicit decoder: JsonDecoder[A]): HRIO[Config, Option[A]] =
    ZIO.serviceWithZIO[Config](_.readOpt[A](jsonPath*)(decoder))

  def readLayer[A: Tag](jsonPath: String*)(implicit decoder: JsonDecoder[A]): HRLayer[Config, A] =
    ZLayer.fromZIO { Config.read[A](jsonPath*)(decoder) }

  // =====| ZIOs |=====

  def fromJarResource(path: String): HTask[Config] =
    for {
      stream <-
        ZIO
          .hAttempt { Option(this.getClass.getClassLoader.getResourceAsStream(path)) }
          .someOrFail(HError.InternalDefect(s"No such jar resource: $path"))
      string <- ZIO.hAttempt(new String(stream.readAllBytes()))
      config <- string.fromJson[Config] match {
        case Right(config) => ZIO.succeed(config)
        case Left(error)   => ZIO.fail(HError.InternalDefect(s"Unable to decode json config: $error"))
      }
    } yield config

  def fromPath(path: Path): HTask[Config] =
    path.readJson[Json].map(Config(_))

  def fromPathString(path: String): HRIO[FileSystem, Config] =
    Path(path).flatMap(Config.fromPath)

  def fromPaths(paths: List[Path]): HTask[Config] =
    ZIO.foldLeft(paths)(Config.empty) { (c, p) => Config.fromPath(p).map(c + _) }

  def fromPathStrings(paths: List[String]): HRIO[FileSystem, Config] =
    ZIO.foldLeft(paths)(Config.empty) { (c, p) => Config.fromPathString(p).map(c + _) }

  // =====| Layers |=====

  sealed abstract class LayerBuilder[R](make: Config => URIO[R, Config]) {

    final def json(json: Json): URLayer[R, Config] =
      ZLayer.fromZIO { make(Config(json)) }

    final def jsonString(json: String): HRLayer[R, Config] =
      ZLayer.fromZIO { Config.fromJsonString(json).flatMap(make) }

    final def jarResource(path: String): HRLayer[R, Config] =
      ZLayer.fromZIO { Config.fromJarResource(path).flatMap(make) }

    final def path(path: Path): HRLayer[R, Config] =
      ZLayer.fromZIO { Config.fromPath(path).flatMap(make) }

    final def paths(paths: List[Path]): HRLayer[R, Config] =
      ZLayer.fromZIO { Config.fromPaths(paths).flatMap(make) }

    final def pathString(path: String): HRLayer[FileSystem & R, Config] =
      ZLayer.fromZIO { Config.fromPathString(path).flatMap(make) }

    final def pathStrings(paths: List[String]): HRLayer[FileSystem & R, Config] =
      ZLayer.fromZIO { Config.fromPathStrings(paths).flatMap(make) }

  }

  object layer extends LayerBuilder[Any](ZIO.succeed(_)) {

    val empty: ULayer[Config] =
      ZLayer.succeed(Config.empty)

    object prepend extends LayerBuilder[Config](cfg => ZIO.serviceWith[Config](cfg + _))
    object append extends LayerBuilder[Config](cfg => ZIO.serviceWith[Config](_ + cfg))

  }

}
