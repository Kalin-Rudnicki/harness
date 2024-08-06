package harness.zio.config

import cats.syntax.either.*
import cats.syntax.traverse.*
import zio.*
import zio.json.*
import zio.json.ast.Json

object HConfig {

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
