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

  sealed trait Ex
  object Ex {
    final case class A(a: Int) extends Ex
    final case class B(b: String) extends Ex

    implicit val jsonCodec: JsonCodec[Ex] = DeriveJsonCodec.gen

  }

  override val executable: Executable =
    Executable
      .withParser(Parser.unit)
      .withEffect {
        for {
          _ <- Logger.log.info("=====| TestMain |=====")
          jsonStrings = List(
            """{}""",
            """{"A":{"a":1}}""",
            """{"B":{"b":"str"}}""",
            """{"C":{}}""",
          )
          _ <- ZIO.foreachDiscard(jsonStrings) { jsonString => Logger.log.info(s"$jsonString\n${jsonString.fromJson[Ex]}") }
        } yield ()
      }

}
