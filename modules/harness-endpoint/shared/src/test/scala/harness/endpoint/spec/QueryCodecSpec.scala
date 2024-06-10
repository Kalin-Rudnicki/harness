package harness.endpoint.spec

import cats.data.NonEmptyList
import cats.syntax.option.*
import harness.endpoint.error.DecodingFailure
import harness.endpoint.spec.SharedTypes.*
import harness.zio.test.DefaultHarnessSpec
import zio.internal.stacktracer.SourceLocation
import zio.test.*
import zio.test.Assertion.*

object QueryCodecSpec extends DefaultHarnessSpec {

  private def makeSuite[A](name: String)(codec: QueryCodec[A])(schemas0: List[String], schemasN: List[String]*)(cases: (QueryCodec[A] ?=> TestSpec)*)(implicit loc: SourceLocation): TestSpec =
    suite(name)(
      test("schemas") {
        assert(codec.schemas.toList.map(_.map(_.showBasic)))(equalTo(schemas0 :: schemasN.toList))
      } ::
        cases.toList.map(_(using codec)),
    )

  private def makeName(pairs: Seq[(String, List[String])], exp: Any): String =
    pairs.map { case (k, v) => s" $k : ${v.mkString(", ")} " }.mkString("[", "|", s"]  ->  $exp")

  private def makePassingTest[A](pairs: (String, List[String])*)(exp: A)(implicit codec: QueryCodec[A], loc: SourceLocation): TestSpec =
    test(makeName(pairs, exp)) {
      val pairsMap = pairs.toMap
      assert(codec.decode(pairsMap))(isRight(equalTo(exp))) &&
      assert(codec.encode(exp))(equalTo(pairsMap.filter(_._2.nonEmpty)))
    }

  private def makeFailingTest[A](pairs: (String, List[String])*)(exp: DecodingFailure)(implicit codec: QueryCodec[A], loc: SourceLocation): TestSpec =
    test(makeName(pairs, exp)) {
      assert(codec.decode(pairs.toMap))(isLeft(equalTo(exp)))
    }

  // =====|  |=====

  override def testSpec: TestSpec =
    suite("QueryCodecSpec")(
      makeSuite("empty")(QueryCodec.Empty)(
        List(),
      )(
        makePassingTest()(()),
      ),
      makeSuite("required")(QueryCodec.Required[Int]("key"))(
        List("{{key}}"),
      )(
        makeFailingTest("key" -> List("1", "2"))(DecodingFailure(SchemaSource.Query("key") :: Nil, DecodingFailure.Cause.DoesNotAcceptMultiple)),
        makePassingTest("key" -> List("1"))(1),
        makeFailingTest("key" -> List())(DecodingFailure(SchemaSource.Query("key") :: Nil, DecodingFailure.Cause.MissingRequired)),
        makeFailingTest()(DecodingFailure(SchemaSource.Query("key") :: Nil, DecodingFailure.Cause.MissingRequired)),
      ),
      makeSuite("optional")(QueryCodec.Optional[Int]("key"))(
        List("{{key}}?"),
      )(
        makeFailingTest("key" -> List("1", "2"))(DecodingFailure(SchemaSource.Query("key") :: Nil, DecodingFailure.Cause.DoesNotAcceptMultiple)),
        makePassingTest("key" -> List("1"))(1.some),
        makePassingTest("key" -> List())(Option.empty[Int]),
        makePassingTest()(Option.empty[Int]),
      ),
      makeSuite("many")(QueryCodec.Many[Int]("key"))(
        List("{{key}}*"),
      )(
        makePassingTest("key" -> List("1", "2"))(List(1, 2)),
        makePassingTest("key" -> List("1"))(List(1)),
        makePassingTest("key" -> List())(List.empty[Int]),
        makePassingTest()(List.empty[Int]),
      ),
      makeSuite("many-non-empty")(QueryCodec.ManyNonEmpty[Int]("key"))(
        List("{{key}}**"),
      )(
        makePassingTest("key" -> List("1", "2"))(NonEmptyList.of(1, 2)),
        makePassingTest("key" -> List("1"))(NonEmptyList.of(1)),
        makeFailingTest("key" -> List())(DecodingFailure(SchemaSource.Query("key") :: Nil, DecodingFailure.Cause.MissingRequired)),
        makeFailingTest()(DecodingFailure(SchemaSource.Query("key") :: Nil, DecodingFailure.Cause.MissingRequired)),
      ),
      makeSuite("one-of : tier-1")(Tier1Ref.queryCodec)(
        List("{{tier-1-id}}"),
        List("{{tier-1-name}}"),
      )(
        makePassingTest("tier-1-id" -> List(uuid.toString))(Tier1Ref(uuid)),
        makePassingTest("tier-1-name" -> List("T1"))(Tier1Ref("T1")),
      ),
      makeSuite("one-of : tier-2")(Tier2Ref.queryCodec)(
        List("{{tier-2-id}}"),
        List("{{tier-1-id}}", "{{tier-2-name}}"),
        List("{{tier-1-name}}", "{{tier-2-name}}"),
      )(
        makePassingTest("tier-2-id" -> List(uuid.toString))(Tier2Ref(uuid)),
        makePassingTest("tier-1-id" -> List(uuid.toString), "tier-2-name" -> List("T2"))(Tier2Ref(uuid, "T2")),
        makePassingTest("tier-1-name" -> List("T1"), "tier-2-name" -> List("T2"))(Tier2Ref("T1", "T2")),
      ),
      makeSuite("one-of : tier-3")(Tier3Ref.queryCodec)(
        List("{{tier-3-id}}"),
        List("{{tier-2-id}}", "{{tier-3-name}}"),
        List("{{tier-1-id}}", "{{tier-2-name}}", "{{tier-3-name}}"),
        List("{{tier-1-name}}", "{{tier-2-name}}", "{{tier-3-name}}"),
      )(
        makePassingTest("tier-3-id" -> List(uuid.toString))(Tier3Ref(uuid)),
        makePassingTest("tier-2-id" -> List(uuid.toString), "tier-3-name" -> List("T3"))(Tier3Ref(uuid, "T3")),
        makePassingTest("tier-1-id" -> List(uuid.toString), "tier-2-name" -> List("T2"), "tier-3-name" -> List("T3"))(Tier3Ref(uuid, "T2", "T3")),
        makePassingTest("tier-1-name" -> List("T1"), "tier-2-name" -> List("T2"), "tier-3-name" -> List("T3"))(Tier3Ref("T1", "T2", "T3")),
      ),
    )

}
