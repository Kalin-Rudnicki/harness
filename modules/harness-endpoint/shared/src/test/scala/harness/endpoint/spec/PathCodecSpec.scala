package harness.endpoint.spec

import cats.data.NonEmptyList
import harness.endpoint.spec.SharedTypes.*
import harness.zio.test.DefaultHarnessSpec
import zio.internal.stacktracer.SourceLocation
import zio.test.*
import zio.test.Assertion.*

object PathCodecSpec extends DefaultHarnessSpec {

  private def makeSuite[A](name: String)(codec: PathCodec[A])(schemas0: List[String], schemasN: List[String]*)(cases: (PathCodec[A] ?=> TestSpec)*)(implicit loc: SourceLocation): TestSpec =
    suite(name)(
      test("schemas") {
        assert(codec.schemas.toList.map(_.map(_.showBasic)))(equalTo(schemas0 :: schemasN.toList))
      } ::
        cases.toList.map(_(using codec)),
    )

  private def makePassingTest[A](paths: String*)(exp: A)(implicit codec: PathCodec[A], loc: SourceLocation): TestSpec =
    test(paths.mkString("/", "/", s"  ->  $exp")) {
      assert(codec.decodePath(paths.toList))(isSome(equalTo(exp))) &&
      assert(codec.encodePath(exp))(equalTo(paths.toList))
    }

  private def makeFailingTest[A](paths: String*)(implicit codec: PathCodec[A], loc: SourceLocation): TestSpec =
    test(paths.mkString("/", "/", "  ->  [N/A]")) {
      assert(codec.decodePath(paths.toList))(isNone)
    }

  // =====|  |=====

  override def testSpec: TestSpec =
    suite("PathCodecSpec")(
      makeSuite("empty")(PathCodec.Empty)(
        Nil,
      )(
        makePassingTest()(()),
        makeFailingTest("a"),
      ),
      makeSuite("const")(PathCodec.Empty / "a" / "b" / "c")(
        List("a", "b", "c"),
      )(
        makePassingTest("a", "b", "c")(()),
        makeFailingTest("a", "b"),
        makeFailingTest("a", "b", "C"),
        makeFailingTest("a", "b", "c", "d"),
      ),
      makeSuite("param")(PathCodec.Param[Int]("int"))(
        List("{{int}}"),
      )(
        makePassingTest("5")(5),
        makeFailingTest("abc"),
        makeFailingTest("abc", "5"),
        makeFailingTest("5", "abc"),
      ),
      makeSuite("one-of : tier-1")(Tier1Ref.pathCodec)(
        List("tier-1", "{{tier-1-id}}"),
        List("tier-1", "{{tier-1-name}}"),
      )(
        makePassingTest("tier-1", uuid.toString)(Tier1Ref(uuid)),
        makePassingTest("tier-1", "T1")(Tier1Ref("T1")),
      ),
      makeSuite("one-of : tier-2")(Tier2Ref.pathCodec)(
        List("tier-2", "{{tier-2-id}}"),
        List("tier-1", "{{tier-1-id}}", "tier-2", "{{tier-2-name}}"),
        List("tier-1", "{{tier-1-name}}", "tier-2", "{{tier-2-name}}"),
      )(
        makePassingTest("tier-2", uuid.toString)(Tier2Ref(uuid)),
        makePassingTest("tier-1", uuid.toString, "tier-2", "T2")(Tier2Ref(uuid, "T2")),
        makePassingTest("tier-1", "T1", "tier-2", "T2")(Tier2Ref("T1", "T2")),
      ),
      makeSuite("one-of : tier-3")(Tier3Ref.pathCodec)(
        List("tier-3", "{{tier-3-id}}"),
        List("tier-2", "{{tier-2-id}}", "tier-3", "{{tier-3-name}}"),
        List("tier-1", "{{tier-1-id}}", "tier-2", "{{tier-2-name}}", "tier-3", "{{tier-3-name}}"),
        List("tier-1", "{{tier-1-name}}", "tier-2", "{{tier-2-name}}", "tier-3", "{{tier-3-name}}"),
      )(
        makePassingTest("tier-3", uuid.toString)(Tier3Ref(uuid)),
        makePassingTest("tier-2", uuid.toString, "tier-3", "T3")(Tier3Ref(uuid, "T3")),
        makePassingTest("tier-1", uuid.toString, "tier-2", "T2", "tier-3", "T3")(Tier3Ref(uuid, "T2", "T3")),
        makePassingTest("tier-1", "T1", "tier-2", "T2", "tier-3", "T3")(Tier3Ref("T1", "T2", "T3")),
      ),
      makeSuite("rest")(PathCodec.Rest("rest"))(
        List("{{rest}}*"),
      )(
        makePassingTest("a", "b")(List("a", "b")),
        makePassingTest("a")(List("a")),
        makePassingTest()(List.empty[String]),
      ),
      makeSuite("rest-nel")(PathCodec.RestNonEmpty("rest"))(
        List("{{rest}}**"),
      )(
        makePassingTest("a", "b")(NonEmptyList.of("a", "b")),
        makePassingTest("a")(NonEmptyList.of("a")),
        makeFailingTest(),
      ),
      makeSuite("complex")(PathCodec.Const("v2") / Tier1Ref.pathCodec / PathCodec.Const("boolean") / PathCodec.Param[Boolean]("boolean") / PathCodec.RestNonEmpty("rest"))(
        List("v2", "tier-1", "{{tier-1-id}}", "boolean", "{{boolean}}", "{{rest}}**"),
        List("v2", "tier-1", "{{tier-1-name}}", "boolean", "{{boolean}}", "{{rest}}**"),
      )(
        makePassingTest("v2", "tier-1", uuid.toString, "boolean", "true", "a", "b")((Tier1Ref(uuid), true, NonEmptyList.of("a", "b"))),
        makePassingTest("v2", "tier-1", "T1", "boolean", "false", "c")((Tier1Ref("T1"), false, NonEmptyList.of("c"))),
        makeFailingTest("v2", "tier-1", "T1", "boolean", "false"),
        makeFailingTest("tier-1", "T1", "boolean", "false", "c"),
      ),
    )

}
