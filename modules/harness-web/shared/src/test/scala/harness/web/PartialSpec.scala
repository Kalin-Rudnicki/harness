package harness.web

import harness.test.PlainHarnessSpec
import scala.reflect.ClassTag
import zio.json.*
import zio.test.*
import zio.test.Assertion.*

object PartialSpec extends PlainHarnessSpec {

  // =====| Types |=====

  private final case class Klass1(
      int: Partial[Int],
      optInt: Partial[Option[Int]],
  )

  private final case class Klass2(
      str: Partial[String],
      klass1: Partial[Klass1],
  )

  private final case class CodecSpec[A](
      jsonCodec: JsonCodec[A],
      canDecodeFlat: Boolean,
      canDecodeAuto: Boolean,
      flatEncoder: JsonEncoder[A],
      autoEncoder: JsonEncoder[A],
  )

  private final case class AllCodecs[A](
      flat: JsonCodec[A],
      auto: JsonCodec[A],
      `flat / flatOrAuto`: JsonCodec[A],
      `auto / flatOrAuto`: JsonCodec[A],
  )

  // =====| Gens |=====

  private def partialGen[R, A](aGen: Gen[R, A]): Gen[R, Partial[A]] =
    Gen.oneOf(Gen.const(Partial.Unspecified), aGen.map(Partial.Specified.apply))

  private val klass1Gen: Gen[Sized, Klass1] =
    for {
      int <- partialGen(Gen.int)
      optInt <- partialGen(Gen.option(Gen.int))
    } yield Klass1(int, optInt)

  private val klass2Gen: Gen[Sized, Klass2] =
    for {
      str <- partialGen(Gen.alphaNumericStringBounded(5, 10))
      klass1 <- partialGen(klass1Gen)
    } yield Klass2(str, klass1)

  // =====| Test Helpers |=====

  private def makeDecodingTest[A](encoderName: String, aGen: Gen[Sized, A], encoder: JsonEncoder[A], decoder: JsonDecoder[A], canDecode: Boolean): TestSpec =
    test(s"can${if (canDecode) "" else " not"} decode when encoded with '$encoderName' encoder") {
      check(aGen) { t =>
        val exp = if (canDecode) isRight(equalTo(t)) else isLeft
        assert(t.toJson(encoder).fromJson[A](decoder))(exp) &&
        assert(t.toJsonPretty(encoder).fromJson[A](decoder))(exp)
      }
    }

  private def makeCodecSuite[A](codecName: String, aGen: Gen[Sized, A], codecSpec: CodecSpec[A]): TestSpec =
    suite(s"'$codecName' codec")(
      makeDecodingTest("flat", aGen, codecSpec.flatEncoder, codecSpec.jsonCodec.decoder, codecSpec.canDecodeFlat),
      makeDecodingTest("auto", aGen, codecSpec.autoEncoder, codecSpec.jsonCodec.decoder, codecSpec.canDecodeAuto),
      makeDecodingTest("its own", aGen, codecSpec.jsonCodec.encoder, codecSpec.jsonCodec.decoder, true),
    )

  private def makeTypeSuite[A](name: String, aGen: Gen[Sized, A], allCodecs: AllCodecs[A]): TestSpec =
    suite(name)(
      makeCodecSuite("flat", aGen, CodecSpec(allCodecs.flat, true, false, allCodecs.flat.encoder, allCodecs.auto.encoder)),
      makeCodecSuite("auto", aGen, CodecSpec(allCodecs.auto, false, true, allCodecs.flat.encoder, allCodecs.auto.encoder)),
      makeCodecSuite("flat / flatOrAuto", aGen, CodecSpec(allCodecs.`flat / flatOrAuto`, true, true, allCodecs.flat.encoder, allCodecs.auto.encoder)),
      makeCodecSuite("auto / flatOrAuto", aGen, CodecSpec(allCodecs.`auto / flatOrAuto`, true, true, allCodecs.flat.encoder, allCodecs.auto.encoder)),
    )

  // =====| Spec |=====

  override def spec: TestSpec =
    suite("PartialSpec")(
      makeTypeSuite(
        "Klass1",
        klass1Gen,
        AllCodecs[Klass1](
          flat = {
            import Partial.flat.jsonCodec
            DeriveJsonCodec.gen[Klass1]
          },
          auto = {
            import Partial.auto.jsonCodec
            DeriveJsonCodec.gen[Klass1]
          },
          `flat / flatOrAuto` = {
            import Partial.`flat / flatOrAuto`.jsonCodec
            DeriveJsonCodec.gen[Klass1]
          },
          `auto / flatOrAuto` = {
            import Partial.`auto / flatOrAuto`.jsonCodec
            DeriveJsonCodec.gen[Klass1]
          },
        ),
      ),
      makeTypeSuite(
        "Klass2",
        klass2Gen,
        AllCodecs[Klass2](
          flat = {
            import Partial.flat.jsonCodec
            implicit val klass1: JsonCodec[Klass1] = DeriveJsonCodec.gen[Klass1]
            DeriveJsonCodec.gen[Klass2]
          },
          auto = {
            import Partial.auto.jsonCodec
            implicit val klass1: JsonCodec[Klass1] = DeriveJsonCodec.gen[Klass1]
            DeriveJsonCodec.gen[Klass2]
          },
          `flat / flatOrAuto` = {
            import Partial.`flat / flatOrAuto`.jsonCodec
            implicit val klass1: JsonCodec[Klass1] = DeriveJsonCodec.gen[Klass1]
            DeriveJsonCodec.gen[Klass2]
          },
          `auto / flatOrAuto` = {
            import Partial.`auto / flatOrAuto`.jsonCodec
            implicit val klass1: JsonCodec[Klass1] = DeriveJsonCodec.gen[Klass1]
            DeriveJsonCodec.gen[Klass2]
          },
        ),
      ),
    )

}
