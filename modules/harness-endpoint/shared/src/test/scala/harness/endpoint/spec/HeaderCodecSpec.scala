package harness.endpoint.spec

import cats.data.NonEmptyList
import cats.syntax.option.*
import harness.endpoint.error.DecodingFailure
import harness.zio.test.DefaultHarnessSpec
import zio.internal.stacktracer.SourceLocation
import zio.test.*
import zio.test.Assertion.*

object HeaderCodecSpec extends DefaultHarnessSpec {

  private def makeSuite[A](name: String)(codec: HeaderCodec[A])(schemas0: List[String], schemasN: List[String]*)(cases: (HeaderCodec[A] ?=> TestSpec)*)(implicit loc: SourceLocation): TestSpec =
    suite(name)(
      test("schemas") {
        assert(codec.schemas.toList.map(_.map(_.showBasic)))(equalTo(schemas0 :: schemasN.toList))
      } ::
        cases.toList.map(_(using codec)),
    )

  private def makeName(headerPairs: Seq[(String, List[String])], cookiePairs: Seq[(String, String)], exp: Any): String = {
    val headerStr = headerPairs.map { case (k, v) => s" $k : ${v.mkString(", ")} " }.mkString("|")
    val cookieStr = cookiePairs.map { case (k, v) => s" $k : $v " }.mkString("|")
    s"[$headerStr]  +  [$cookieStr]  ->  $exp"
  }

  private def makePassingTest[A](headerPairs: (String, List[String])*)(cookiePairs: (String, String)*)(exp: A)(implicit codec: HeaderCodec[A], loc: SourceLocation): TestSpec =
    test(makeName(headerPairs, cookiePairs, exp)) {
      val headerPairsMap = headerPairs.toMap
      val cookiePairsMap = cookiePairs.toMap
      val (encHeaders, encCookies) = codec.encode(exp)
      assert(codec.decode(headerPairsMap, cookiePairsMap))(isRight(equalTo(exp))) &&
      assert(codec.decode(encHeaders, encCookies))(isRight(equalTo(exp)))
    }

  private def makeFailingTest[A](headerPairs: (String, List[String])*)(cookiePairs: (String, String)*)(exp: DecodingFailure)(implicit codec: HeaderCodec[A], loc: SourceLocation): TestSpec =
    test(makeName(headerPairs, cookiePairs, exp)) {
      assert(codec.decode(headerPairs.toMap, cookiePairs.toMap))(isLeft(equalTo(exp)))
    }

  // =====|  |=====

  private val key = "HEADER-1"

  override def testSpec: TestSpec =
    suite("HeaderCodecSpec")(
      makeSuite("empty")(HeaderCodec.Empty)(
        List(),
      )(
        makePassingTest()()(()),
      ),
      suite("header")(
        makeSuite("header-required")(HeaderCodec.HeaderRequired[Int](key))(
          List("{{HEADER-1}}"),
        )(
          // with cookie
          makeFailingTest(key -> List("1", "2"))(key -> "3")(DecodingFailure(SchemaSource.Header(key) :: Nil, DecodingFailure.Cause.ProvidedAsHeaderAndCookie)),
          makeFailingTest(key -> List("1"))(key -> "3")(DecodingFailure(SchemaSource.Header(key) :: Nil, DecodingFailure.Cause.ProvidedAsHeaderAndCookie)),
          makeFailingTest(key -> List())(key -> "3")(DecodingFailure(SchemaSource.Header(key) :: Nil, DecodingFailure.Cause.ProvidedAsHeaderAndCookie)),
          makeFailingTest()(key -> "3")(DecodingFailure(SchemaSource.Header(key) :: Nil, DecodingFailure.Cause.MissingRequired)),
          // without cookie
          makeFailingTest(key -> List("1", "2"))()(DecodingFailure(SchemaSource.Header(key) :: Nil, DecodingFailure.Cause.DoesNotAcceptMultiple)),
          makePassingTest(key -> List("1"))()(1),
          makeFailingTest(key -> List())()(DecodingFailure(SchemaSource.Header(key) :: Nil, DecodingFailure.Cause.MissingRequired)),
          makeFailingTest()()(DecodingFailure(SchemaSource.Header(key) :: Nil, DecodingFailure.Cause.MissingRequired)),
        ),
        makeSuite("header-optional")(HeaderCodec.HeaderOptional[Int](key))(
          List("{{HEADER-1}}?"),
        )(
          // with cookie
          makeFailingTest(key -> List("1", "2"))(key -> "3")(DecodingFailure(SchemaSource.Header(key) :: Nil, DecodingFailure.Cause.ProvidedAsHeaderAndCookie)),
          makeFailingTest(key -> List("1"))(key -> "3")(DecodingFailure(SchemaSource.Header(key) :: Nil, DecodingFailure.Cause.ProvidedAsHeaderAndCookie)),
          makeFailingTest(key -> List())(key -> "3")(DecodingFailure(SchemaSource.Header(key) :: Nil, DecodingFailure.Cause.ProvidedAsHeaderAndCookie)),
          makePassingTest()(key -> "3")(Option.empty[Int]),
          // without cookie
          makeFailingTest(key -> List("1", "2"))()(DecodingFailure(SchemaSource.Header(key) :: Nil, DecodingFailure.Cause.DoesNotAcceptMultiple)),
          makePassingTest(key -> List("1"))()(1.some),
          makePassingTest(key -> List())()(Option.empty[Int]),
          makePassingTest()()(Option.empty[Int]),
        ),
        makeSuite("header-many")(HeaderCodec.HeaderMany[Int](key))(
          List("{{HEADER-1}}*"),
        )(
          // with cookie
          makeFailingTest(key -> List("1", "2"))(key -> "3")(DecodingFailure(SchemaSource.Header(key) :: Nil, DecodingFailure.Cause.ProvidedAsHeaderAndCookie)),
          makeFailingTest(key -> List("1"))(key -> "3")(DecodingFailure(SchemaSource.Header(key) :: Nil, DecodingFailure.Cause.ProvidedAsHeaderAndCookie)),
          makeFailingTest(key -> List())(key -> "3")(DecodingFailure(SchemaSource.Header(key) :: Nil, DecodingFailure.Cause.ProvidedAsHeaderAndCookie)),
          makePassingTest()(key -> "3")(List.empty[Int]),
          // without cookie
          makePassingTest(key -> List("1", "2"))()(List(1, 2)),
          makePassingTest(key -> List("1"))()(List(1)),
          makePassingTest(key -> List())()(List.empty[Int]),
          makePassingTest()()(List.empty[Int]),
        ),
        makeSuite("header-many-non-empty")(HeaderCodec.HeaderManyNonEmpty[Int](key))(
          List("{{HEADER-1}}**"),
        )(
          // with cookie
          makeFailingTest(key -> List("1", "2"))(key -> "3")(DecodingFailure(SchemaSource.Header(key) :: Nil, DecodingFailure.Cause.ProvidedAsHeaderAndCookie)),
          makeFailingTest(key -> List("1"))(key -> "3")(DecodingFailure(SchemaSource.Header(key) :: Nil, DecodingFailure.Cause.ProvidedAsHeaderAndCookie)),
          makeFailingTest(key -> List())(key -> "3")(DecodingFailure(SchemaSource.Header(key) :: Nil, DecodingFailure.Cause.ProvidedAsHeaderAndCookie)),
          makeFailingTest()(key -> "3")(DecodingFailure(SchemaSource.Header(key) :: Nil, DecodingFailure.Cause.MissingRequired)),
          // without cookie
          makePassingTest(key -> List("1", "2"))()(NonEmptyList.of(1, 2)),
          makePassingTest(key -> List("1"))()(NonEmptyList.of(1)),
          makeFailingTest(key -> List())()(DecodingFailure(SchemaSource.Header(key) :: Nil, DecodingFailure.Cause.MissingRequired)),
          makeFailingTest()()(DecodingFailure(SchemaSource.Header(key) :: Nil, DecodingFailure.Cause.MissingRequired)),
        ),
      ),
      suite("cookie")(
        makeSuite("cookie-required")(HeaderCodec.CookieRequired[Int](key))(
          List("{{HEADER-1}}"),
        )(
          // with cookie
          makeFailingTest(key -> List("1", "2"))(key -> "3")(DecodingFailure(SchemaSource.Cookie(key) :: Nil, DecodingFailure.Cause.ProvidedAsHeaderAndCookie)),
          makeFailingTest(key -> List("1"))(key -> "3")(DecodingFailure(SchemaSource.Cookie(key) :: Nil, DecodingFailure.Cause.ProvidedAsHeaderAndCookie)),
          makeFailingTest(key -> List())(key -> "3")(DecodingFailure(SchemaSource.Cookie(key) :: Nil, DecodingFailure.Cause.ProvidedAsHeaderAndCookie)),
          makePassingTest()(key -> "3")(3),
          // without cookie
          makeFailingTest(key -> List("1", "2"))()(DecodingFailure(SchemaSource.Cookie(key) :: Nil, DecodingFailure.Cause.MissingRequired)),
          makeFailingTest(key -> List("1"))()(DecodingFailure(SchemaSource.Cookie(key) :: Nil, DecodingFailure.Cause.MissingRequired)),
          makeFailingTest(key -> List())()(DecodingFailure(SchemaSource.Cookie(key) :: Nil, DecodingFailure.Cause.MissingRequired)),
          makeFailingTest()()(DecodingFailure(SchemaSource.Cookie(key) :: Nil, DecodingFailure.Cause.MissingRequired)),
        ),
        makeSuite("cookie-optional")(HeaderCodec.CookieOptional[Int](key))(
          List("{{HEADER-1}}?"),
        )(
          // with cookie
          makeFailingTest(key -> List("1", "2"))(key -> "3")(DecodingFailure(SchemaSource.Cookie(key) :: Nil, DecodingFailure.Cause.ProvidedAsHeaderAndCookie)),
          makeFailingTest(key -> List("1"))(key -> "3")(DecodingFailure(SchemaSource.Cookie(key) :: Nil, DecodingFailure.Cause.ProvidedAsHeaderAndCookie)),
          makeFailingTest(key -> List())(key -> "3")(DecodingFailure(SchemaSource.Cookie(key) :: Nil, DecodingFailure.Cause.ProvidedAsHeaderAndCookie)),
          makePassingTest()(key -> "3")(3.some),
          // without cookie
          makePassingTest(key -> List("1", "2"))()(Option.empty[Int]),
          makePassingTest(key -> List("1"))()(Option.empty[Int]),
          makePassingTest(key -> List())()(Option.empty[Int]),
          makePassingTest()()(Option.empty[Int]),
        ),
      ),
      suite("header-or-cookie")(
        makeSuite("header-or-cookie-required")(HeaderCodec.HeaderOrCookieRequired[Int](key))(
          List("{{HEADER-1}}"),
        )(
          // with cookie
          makeFailingTest(key -> List("1", "2"))(key -> "3")(DecodingFailure(SchemaSource.HeaderOrCookie(key) :: Nil, DecodingFailure.Cause.ProvidedAsHeaderAndCookie)),
          makeFailingTest(key -> List("1"))(key -> "3")(DecodingFailure(SchemaSource.HeaderOrCookie(key) :: Nil, DecodingFailure.Cause.ProvidedAsHeaderAndCookie)),
          makeFailingTest(key -> List())(key -> "3")(DecodingFailure(SchemaSource.HeaderOrCookie(key) :: Nil, DecodingFailure.Cause.ProvidedAsHeaderAndCookie)),
          makePassingTest()(key -> "3")(3),
          // without cookie
          makeFailingTest(key -> List("1", "2"))()(DecodingFailure(SchemaSource.HeaderOrCookie(key) :: Nil, DecodingFailure.Cause.DoesNotAcceptMultiple)),
          makePassingTest(key -> List("1"))()(1),
          makeFailingTest(key -> List())()(DecodingFailure(SchemaSource.HeaderOrCookie(key) :: Nil, DecodingFailure.Cause.MissingRequired)),
          makeFailingTest()()(DecodingFailure(SchemaSource.HeaderOrCookie(key) :: Nil, DecodingFailure.Cause.MissingRequired)),
        ),
        makeSuite("header-or-cookie-optional")(HeaderCodec.HeaderOrCookieOptional[Int](key))(
          List("{{HEADER-1}}?"),
        )(
          // with cookie
          makeFailingTest(key -> List("1", "2"))(key -> "3")(DecodingFailure(SchemaSource.HeaderOrCookie(key) :: Nil, DecodingFailure.Cause.ProvidedAsHeaderAndCookie)),
          makeFailingTest(key -> List("1"))(key -> "3")(DecodingFailure(SchemaSource.HeaderOrCookie(key) :: Nil, DecodingFailure.Cause.ProvidedAsHeaderAndCookie)),
          makeFailingTest(key -> List())(key -> "3")(DecodingFailure(SchemaSource.HeaderOrCookie(key) :: Nil, DecodingFailure.Cause.ProvidedAsHeaderAndCookie)),
          makePassingTest()(key -> "3")(3.some),
          // without cookie
          makeFailingTest(key -> List("1", "2"))()(DecodingFailure(SchemaSource.HeaderOrCookie(key) :: Nil, DecodingFailure.Cause.DoesNotAcceptMultiple)),
          makePassingTest(key -> List("1"))()(1.some),
          makePassingTest(key -> List())()(Option.empty[Int]),
          makePassingTest()()(Option.empty[Int]),
        ),
      ),
    )

}
