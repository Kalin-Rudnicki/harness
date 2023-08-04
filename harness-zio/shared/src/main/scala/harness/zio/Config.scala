package harness.zio

import cats.data.NonEmptyList
import harness.core.*
import scala.annotation.tailrec
import zio.*
import zio.json.*
import zio.json.ast.Json

final class Config(private val json: Json) { self =>

  def +(other: Config): Config = Config(self.json.merge(other.json))

  def read[A](jsonPath: String*)(implicit decoder: JsonDecoder[A]): HTask[A] = {
    @tailrec
    def loop(jsonPath: List[String], json: Json, rSeenJsonPath: List[String]): HTask[A] = {
      inline def fail(msg: String): HTask[Nothing] =
        ZIO.fail(HError.InternalDefect(s"Unable to decode at path ${rSeenJsonPath.reverse.mkString("[", ".", "]")}: $msg"))

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
          // NOTE : `fromJsonAST` is not used, because ZIO json doesnt properly decode missing values to None with that function
          decoder.decodeJson(json.toString) match {
            case Right(value) => ZIO.succeed(value)
            case Left(error)  => fail(error)
          }
      }
    }

    loop(jsonPath.toList, json, Nil)
  }

}
object Config {

  val empty: Config =
    Config(Json.Obj())

  def fromJson(json: Json): Config =
    Config(json)

  // =====| API |=====

  def read[A](jsonPath: String*)(implicit decoder: JsonDecoder[A]): HRIO[Config, A] =
    ZIO.serviceWithZIO[Config](_.read[A](jsonPath*)(decoder))

  def readLayer[A: Tag](jsonPath: String*)(implicit decoder: JsonDecoder[A]): HRLayer[Config, A] =
    ZLayer.fromZIO { Config.read[A](jsonPath*)(decoder) }

  // =====| ZIOs |=====

  def fromPath(path: Path): HTask[Config] =
    path.readJson[Json].map(Config(_))

  def fromPathString(path: String): HRIO[FileSystem, Config] =
    Path(path).flatMap(Config.fromPath)

  def fromPaths(paths: List[Path]): HTask[Config] =
    ZIO.foldLeft(paths)(Config.empty) { (c, p) => Config.fromPath(p).map(c + _) }

  def fromPathStrings(paths: List[String]): HRIO[FileSystem, Config] =
    ZIO.foldLeft(paths)(Config.empty) { (c, p) => Config.fromPathString(p).map(c + _) }

  // =====| Layers |=====

  val emptyLayer: ULayer[Config] =
    ZLayer.succeed(Config.empty)

  def fromJsonLayer(json: Json): ULayer[Config] =
    ZLayer.succeed(Config(json))

  def fromPathLayer(path: Path): HTaskLayer[Config] =
    ZLayer.fromZIO { Config.fromPath(path) }

  def fromPathStringLayer(path: String): HRLayer[FileSystem, Config] =
    ZLayer.fromZIO { Config.fromPathString(path) }

  def fromPathsLayer(paths: List[Path]): HTaskLayer[Config] =
    ZLayer.fromZIO { Config.fromPaths(paths) }

  def fromPathStringsLayer(paths: List[String]): HRLayer[FileSystem, Config] =
    ZLayer.fromZIO { Config.fromPathStrings(paths) }

  def addPathLayer(path: Path): HRLayer[Config, Config] =
    ZLayer.fromZIO {
      for {
        a <- ZIO.service[Config]
        b <- Config.fromPath(path)
      } yield a + b
    }

  def addPathStringLayer(path: String): HRLayer[Config & FileSystem, Config] =
    ZLayer.fromZIO {
      for {
        a <- ZIO.service[Config]
        b <- Config.fromPathString(path)
      } yield a + b
    }

  def addPathsLayer(paths: List[Path]): HRLayer[Config, Config] =
    ZLayer.fromZIO {
      for {
        a <- ZIO.service[Config]
        b <- Config.fromPaths(paths)
      } yield a + b
    }

  def addPathsStringLayer(paths: List[String]): HRLayer[Config & FileSystem, Config] =
    ZLayer.fromZIO {
      for {
        a <- ZIO.service[Config]
        b <- Config.fromPathStrings(paths)
      } yield a + b
    }

}
