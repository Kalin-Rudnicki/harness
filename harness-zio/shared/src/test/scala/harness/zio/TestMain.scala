package harness.zio

import cats.data.NonEmptyList
import cats.syntax.either.*
import cats.syntax.option.*
import harness.cli.Parser
import harness.core.HError
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
          res1 <- ZIO.succeed("123").hErrorOrToEither.exit
          res2 <- ZIO.fail(HError.UserError("456")).hErrorOrToEither.exit
          res3 <- ZIO.fail(HError.Or("789")).hErrorOrToEither.exit
          _ <- Logger.log.info(s"res1: $res1")
          _ <- Logger.log.info(s"res2: $res2")
          _ <- Logger.log.info(s"res3: $res3")
          trace <- ZIO.stackTrace
          _ <- Logger.log.detailed(trace)
          _ <- Logger.log.detailed(trace.stackTrace.size)
          _ <- Logger.log.detailed(trace.stackTrace)
          _ <- Logger.log.detailed(trace.stackTrace.mkString("\n"))
          _ <- Logger.log.detailed(trace.prettyPrint)
        } yield ()
      }

}
