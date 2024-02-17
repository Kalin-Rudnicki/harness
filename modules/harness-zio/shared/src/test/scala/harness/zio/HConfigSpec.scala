package harness.zio

import cats.syntax.option.*
import harness.zio.test.*
import zio.*
import zio.json.*
import zio.json.ast.Json
import zio.test.*
import zio.test.Assertion.*

object HConfigSpec extends HarnessSpec[HConfig] {

  private final case class Cfg(
      a: Int,
      b: String,
      c: Option[Int],
  )
  private object Cfg {
    implicit val jsonCodec: JsonCodec[Cfg] = DeriveJsonCodec.gen
  }

  private val cfg1: Cfg = Cfg(1, "a", None)
  private val cfg2: Cfg = Cfg(2, "b", 2.some)

  override val layer: ZLayer[HarnessEnv & Scope, Any, HConfig] =
    HConfig.layer.json(
      Json.Obj(
        "key-1" -> cfg1.toJsonAST.toOption.get,
        "key-2" -> Json.Obj(
          "nested" -> cfg2.toJsonAST.toOption.get,
        ),
      ),
    ) >>>
      HConfig.layer.append.jarResource("config-spec.json")

  private def makeTest(i: Int)(path: String*)(assertion: Assertion[Either[Any, Cfg]]): TestSpec =
    test(s"test-$i") {
      assertZIO(HConfig.read[Cfg](path*).either)(assertion)
    }

  override def spec: TestSpec =
    suite("ConfigSpec")(
      makeTest(1)("key-1")(isRight(equalTo(cfg1))),
      makeTest(2)("key-2", "nested")(isRight(equalTo(cfg2))),
      makeTest(3)("key-3")(isLeft),
      makeTest(4)("key-4")(isRight(equalTo(Cfg(4, "d", None)))),
    )

}
