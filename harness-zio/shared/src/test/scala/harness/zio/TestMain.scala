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

  private def runTest(path: String*): URIO[Config & Logger, Unit] =
    (for {
      _ <- Logger.log.info("")
      v <- Config.read[Ex](path*)
      _ <- Logger.log.info(v)
    } yield ()).collapseCause.catchAll(e => Logger.log.info(e.fullInternalMessage))

  override val executable: Executable =
    Executable
      .withParser(Parser.unit)
      .withLayer {
        Config.layer.json(
          Json.Obj(
            "case-1" -> Ex(1, 2.some).toJsonAST.toOption.get,
            "case-2" -> Ex(1, None).toJsonAST.toOption.get,
          ),
        )
      }
      .withEffect {
        for {
          _ <- Logger.log.info("=====| TestMain |=====")
          _ <- runTest("case-1")
          _ <- runTest("case-2")
          _ <- runTest("case-3")
          res <- ZIO.hAttempt(getClass.getClassLoader.getResources("/"))
          resList = res.asIterator().asScala.toList
          _ <- Logger.log.info(resList.size)
          _ <- Logger.log.info(resList.mkString("\n"))
        } yield ()
      }

}
