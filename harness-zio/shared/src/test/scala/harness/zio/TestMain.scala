package harness.zio

import cats.data.NonEmptyList
import cats.syntax.either.*
import cats.syntax.option.*
import harness.cli.Parser
import scala.jdk.CollectionConverters.*
import zio.*
import zio.json.*
import zio.json.ast.Json

object TestMain extends ExecutableApp {

  final case class Ex(
      a: Int,
      b: Option[Int],
  )
  object Ex {
    implicit val jsonCodec: JsonCodec[Ex] = DeriveJsonCodec.gen
  }

  override val executable: Executable =
    Executable
      .withParser(Parser.unit)
      .withEffect {
        for {
          _ <- Logger.log.info("=====| TestMain |=====")
          cfg <- ZIO.service[Config]
          _ <- Logger.log.info(cfg.configJson.toJsonPretty)
        } yield ()
      }

}
