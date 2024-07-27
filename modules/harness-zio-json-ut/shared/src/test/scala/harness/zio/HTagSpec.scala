package harness.zio

import harness.zio.test.*
import zio.*
import zio.internal.stacktracer.SourceLocation
import zio.test.*

final class ExampleType1
final class ExampleType2[A]
final class ExampleType3[A, B, C]

object HTagSpec extends DefaultHarnessSpec {

  object inner1 {
    object inner2 {
      final class ExampleType4
      final class ExampleType5[A]
      final class ExampleType6[A, B, C]
    }
  }

  private def makeSuite(name: String)(hTag: => HTag[?])(
      prefixAll: String,
      prefixAllNoGenerics: String,
      prefixObject: String,
      prefixObjectNoGenerics: String,
      prefixNone: String,
      prefixNoneNoGenerics: String,
  )(implicit loc: SourceLocation): TestSpec = {
    def makeTest(generics: Boolean, p: HTag.PackagePrefix, exp: String): TestSpec =
      test(s"[generics=$generics, package-prefix=$p]") {
        assertTrue(hTag.prefix(generics, p) == exp)
      }

    suite(name)(
      makeTest(true, HTag.PackagePrefix.All, prefixAll),
      makeTest(true, HTag.PackagePrefix.Object, prefixObject),
      makeTest(true, HTag.PackagePrefix.None, prefixNone),
      makeTest(false, HTag.PackagePrefix.All, prefixAllNoGenerics),
      makeTest(false, HTag.PackagePrefix.Object, prefixObjectNoGenerics),
      makeTest(false, HTag.PackagePrefix.None, prefixNoneNoGenerics),
    )
  }

  override def testSpec: TestSpec =
    suite("HTagSpec")(
      suite("Tag")(
        makeSuite("ExampleType1")(HTag.usingTag[ExampleType1])(
          "harness.zio.ExampleType1",
          "harness.zio.ExampleType1",
          "ExampleType1",
          "ExampleType1",
          "ExampleType1",
          "ExampleType1",
        ),
        makeSuite("ExampleType2 - Unit")(HTag.usingTag[ExampleType2[Unit]])(
          "harness.zio.ExampleType2[scala.Unit]",
          "harness.zio.ExampleType2",
          "ExampleType2[Unit]",
          "ExampleType2",
          "ExampleType2[Unit]",
          "ExampleType2",
        ),
        makeSuite("ExampleType2 - Either")(HTag.usingTag[ExampleType2[Either[Int, String]]])(
          "harness.zio.ExampleType2[scala.util.Either[scala.Int, java.lang.String]]",
          "harness.zio.ExampleType2",
          "ExampleType2[Either[Int, String]]",
          "ExampleType2",
          "ExampleType2[Either[Int, String]]",
          "ExampleType2",
        ),
        makeSuite("ExampleType2 - Either Either")(HTag.usingTag[ExampleType2[Either[Int, Either[Boolean, String]]]])(
          "harness.zio.ExampleType2[scala.util.Either[scala.Int, scala.util.Either[scala.Boolean, java.lang.String]]]",
          "harness.zio.ExampleType2",
          "ExampleType2[Either[Int, Either[Boolean, String]]]",
          "ExampleType2",
          "ExampleType2[Either[Int, Either[Boolean, String]]]",
          "ExampleType2",
        ),
        makeSuite("ExampleType5 - ExampleType6")(HTag.usingTag[inner1.inner2.ExampleType5[inner1.inner2.ExampleType6[Int, String, Boolean]]])(
          "harness.zio.HTagSpec.inner1.inner2.ExampleType5[harness.zio.HTagSpec.inner1.inner2.ExampleType6[scala.Int, java.lang.String, scala.Boolean]]",
          "harness.zio.HTagSpec.inner1.inner2.ExampleType5",
          "HTagSpec.inner1.inner2.ExampleType5[HTagSpec.inner1.inner2.ExampleType6[Int, String, Boolean]]",
          "HTagSpec.inner1.inner2.ExampleType5",
          "ExampleType5[ExampleType6[Int, String, Boolean]]",
          "ExampleType5",
        ),
        makeSuite("ExampleType6")(HTag.usingTag[inner1.inner2.ExampleType6[Int, String, Boolean]])(
          "harness.zio.HTagSpec.inner1.inner2.ExampleType6[scala.Int, java.lang.String, scala.Boolean]",
          "harness.zio.HTagSpec.inner1.inner2.ExampleType6",
          "HTagSpec.inner1.inner2.ExampleType6[Int, String, Boolean]",
          "HTagSpec.inner1.inner2.ExampleType6",
          "ExampleType6[Int, String, Boolean]",
          "ExampleType6",
        ),
      ),
      suite("ClassTag")(
        makeSuite("ExampleType1")(HTag.usingClassTag[ExampleType1])(
          "harness.zio.ExampleType1",
          "harness.zio.ExampleType1",
          "ExampleType1",
          "ExampleType1",
          "ExampleType1",
          "ExampleType1",
        ),
        makeSuite("ExampleType2 - Unit")(HTag.usingClassTag[ExampleType2[Unit]])(
          "harness.zio.ExampleType2[A]",
          "harness.zio.ExampleType2",
          "ExampleType2[A]",
          "ExampleType2",
          "ExampleType2[A]",
          "ExampleType2",
        ),
        makeSuite("ExampleType2 - Either")(HTag.usingClassTag[ExampleType2[Either[Int, String]]])(
          "harness.zio.ExampleType2[A]",
          "harness.zio.ExampleType2",
          "ExampleType2[A]",
          "ExampleType2",
          "ExampleType2[A]",
          "ExampleType2",
        ),
        makeSuite("ExampleType2 - Either Either")(HTag.usingClassTag[ExampleType2[Either[Int, Either[Boolean, String]]]])(
          "harness.zio.ExampleType2[A]",
          "harness.zio.ExampleType2",
          "ExampleType2[A]",
          "ExampleType2",
          "ExampleType2[A]",
          "ExampleType2",
        ),
        makeSuite("ExampleType5 - ExampleType6")(HTag.usingClassTag[inner1.inner2.ExampleType5[inner1.inner2.ExampleType6[Int, String, Boolean]]])(
          "harness.zio.HTagSpec.inner1.inner2.ExampleType5[A]",
          "harness.zio.HTagSpec.inner1.inner2.ExampleType5",
          "HTagSpec.inner1.inner2.ExampleType5[A]",
          "HTagSpec.inner1.inner2.ExampleType5",
          "ExampleType5[A]",
          "ExampleType5",
        ),
        makeSuite("ExampleType6")(HTag.usingClassTag[inner1.inner2.ExampleType6[Int, String, Boolean]])(
          "harness.zio.HTagSpec.inner1.inner2.ExampleType6[A, B, C]",
          "harness.zio.HTagSpec.inner1.inner2.ExampleType6",
          "HTagSpec.inner1.inner2.ExampleType6[A, B, C]",
          "HTagSpec.inner1.inner2.ExampleType6",
          "ExampleType6[A, B, C]",
          "ExampleType6",
        ),
        suite("fromName")(
          makeSuite("example1")(HTag.fromName("a.B::C").withGenerics(HTag.fromName("d.E$F")))(
            "a.B.C[d.E.F]",
            "a.B.C",
            "B.C[E.F]",
            "B.C",
            "C[F]",
            "C",
          ),
        ),
        suite("misc")(
          test("roundTrip") {
            val tag1 = Tag[inner1.inner2.ExampleType5[inner1.inner2.ExampleType6[Int, String, Boolean]]]
            val hTag1 = tag1.toHTag
            val tag2 = hTag1.toTag
            val hTag2 = tag2.toHTag
            assertTrue(hTag2 == hTag1)
          },
        ),
      ),
    )

}
