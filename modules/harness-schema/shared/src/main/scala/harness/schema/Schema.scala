package harness.schema

import cats.data.NonEmptyList
import cats.syntax.either.*
import cats.syntax.option.*
import harness.core.*
import harness.deriving.*
import harness.zio.json.*
import java.util.UUID
import scala.reflect.ClassTag
import zio.Tag
import zio.json.*

sealed trait Schema[A] { self =>

  val tag: Tag[A]

  def tiemap[B: Tag](to: A => Either[String, B], from: B => A): Schema[B]
  def timap[B: Tag](to: A => B, from: B => A): Schema[B]

  final def encode(value: A): String = self match {
    case schema: JsonSchema[A] => value.toJson(using schema.codec.encoder)
    case schema: RawSchema[A]  => schema.codec.encoder.encode(value)
  }

  final def decode(string: String): Either[String, A] = self match {
    case schema: JsonSchema[A] => string.fromJson[A](using schema.codec.decoder)
    case schema: RawSchema[A]  => schema.codec.decoder.decode(string)
  }

  final val ref: SchemaRef = SchemaRef(tag, UUID.randomUUID)

}
object Schema {

  inline def apply[A](implicit schema: Schema[A]): Schema[A] = schema

}

// =====| Raw Schema |=====

sealed trait RawSchema[A] extends Schema[A] { self =>

  val codec: StringCodec[A]

  override def tiemap[B: Tag](to: A => Either[String, B], from: B => A): RawSchema[B] =
    self match {
      case RawSchema.Str(_, codec, enumValues) => RawSchema.Str(Tag[B], codec.iemap(to, from), enumValues)
    }

  override def timap[B: Tag](to: A => B, from: B => A): RawSchema[B] = tiemap(to(_).asRight, from)

}
object RawSchema {

  inline def apply[A](implicit schema: RawSchema[A]): RawSchema[A] = schema

  final case class Str[A](tag: Tag[A], codec: StringCodec[A], enumValues: Option[List[String]]) extends RawSchema[A]

  // =====| Instances |=====

  implicit def encodedStringSchema[A: StringEncoder: StringDecoder: Tag]: RawSchema[A] =
    Str(Tag[A], StringCodec(StringEncoder[A], StringDecoder[A]), None)

  implicit def enumSchema[E <: Enum[E]: Tag: ClassTag](implicit ewe: Enum.WithEnc[E, String]): RawSchema[E] =
    Str(Tag[E], StringCodec(StringEncoder.`enum`[E, String], StringDecoder.`enum`[E, String]), ewe.values.toList.map(ewe.encode).some)

}

// =====| Json Schema |=====

sealed trait JsonSchema[A] extends Schema[A] { self =>

  val codec: JsonCodec[A]

  override def tiemap[B: Tag](to: A => Either[String, B], from: B => A): JsonSchema[B] =
    self match {
      case JsonSchema.JsonNum(_, codec) =>
        JsonSchema.JsonNum(Tag[B], codec.transformOrFail(to, from))
      case JsonSchema.JsonBool(_, codec) =>
        JsonSchema.JsonBool(Tag[B], codec.transformOrFail(to, from))
      case JsonSchema.JsonStr(_, codec, enumValues) =>
        JsonSchema.JsonStr(Tag[B], codec.transformOrFail(to, from), enumValues)
      case JsonSchema.JsonArr(_, codec, elem) =>
        JsonSchema.JsonArr(Tag[B], codec.transformOrFail(to, from), elem)
      case JsonSchema.NotRequired(_, codec, elem, canBeNull, canBeMissing) =>
        JsonSchema.NotRequired(Tag[B], codec.transformOrFail(to, from), elem, canBeNull, canBeMissing)
      case JsonSchema.ProductJsonObj(_, codec, name, elems) =>
        JsonSchema.ProductJsonObj(Tag[B], codec.transformOrFail(to, from), name, elems)
      case JsonSchema.SumJsonObj(_, codec, discriminator, options) =>
        JsonSchema.SumJsonObj(Tag[B], codec.transformOrFail(to, from), discriminator, options)
    }

  override def timap[B: Tag](to: A => B, from: B => A): JsonSchema[B] = tiemap(to(_).asRight, from)

}
object JsonSchema extends K0.Derivable[JsonSchema] {

  inline def apply[A](implicit schema: JsonSchema[A]): JsonSchema[A] = schema

  final case class JsonNum[A](tag: Tag[A], codec: JsonCodec[A]) extends JsonSchema[A]
  final case class JsonBool[A](tag: Tag[A], codec: JsonCodec[A]) extends JsonSchema[A]
  final case class JsonStr[A](tag: Tag[A], codec: JsonCodec[A], enumValues: Option[List[String]]) extends JsonSchema[A]

  final case class NotRequired[A](
      tag: Tag[A],
      codec: JsonCodec[A],
      elem: JsonSchema[?],
      canBeNull: Boolean,
      canBeMissing: Boolean,
  ) extends JsonSchema[A]

  final case class JsonArr[A](
      tag: Tag[A],
      codec: JsonCodec[A],
      elem: JsonSchema[?],
  ) extends JsonSchema[A]

  sealed trait JsonObj[A] extends JsonSchema[A]

  final case class ProductField(
      name: String,
      schema: JsonSchema[?],
  )

  final case class ProductJsonObj[A](
      tag: Tag[A],
      codec: JsonCodec[A],
      name: String,
      elems: Lazy[List[ProductField]],
  ) extends JsonObj[A]

  final case class SumOption[A](option: ProductJsonObj[A]) {
    val ref: SchemaRef = SchemaRef(option.tag, UUID.randomUUID)
  }

  final case class SumJsonObj[A](
      tag: Tag[A],
      codec: JsonCodec[A],
      discriminator: Option[String],
      options: List[SumOption[?]],
  ) extends JsonObj[A]

  // =====| Basic Instances |=====

  // --- Num ---

  private def makeNum[A: Tag](codec: JsonCodec[A]): JsonSchema.JsonNum[A] =
    JsonNum(Tag[A], codec)

  implicit val byteSchema: JsonSchema[Byte] = makeNum(JsonCodec.byte)
  implicit val shortSchema: JsonSchema[Short] = makeNum(JsonCodec.short)
  implicit val intSchema: JsonSchema[Int] = makeNum(JsonCodec.int)
  implicit val longSchema: JsonSchema[Long] = makeNum(JsonCodec.long)
  implicit val bigIntSchema: JsonSchema[BigInt] = makeNum(JsonCodec.scalaBigInt)

  implicit val floatSchema: JsonSchema[Float] = makeNum(JsonCodec.float)
  implicit val doubleSchema: JsonSchema[Double] = makeNum(JsonCodec.double)
  implicit val bigDecimalSchema: JsonSchema[BigDecimal] = makeNum(JsonCodec.scalaBigDecimal)

  // --- Bool ---

  implicit val booleanSchema: JsonSchema[Boolean] = JsonBool(Tag[Boolean], JsonCodec.boolean)

  // --- Str ---

  implicit val charSchema: JsonSchema[Char] = JsonStr(Tag[Char], JsonCodec.char, None)
  implicit val stringSchema: JsonSchema[String] = JsonStr(Tag[String], JsonCodec.string, None)

  implicit def enumSchema[E <: Enum[E]: Tag: ClassTag](implicit ewe: Enum.WithEnc[E, String]): JsonSchema[E] =
    JsonStr(Tag[E], JsonCodec.`enum`[E, String], ewe.values.toList.map(ewe.encode).some)

  // TODO (KR) : add specific instances
  implicit def encodedStringSchema[A: StringEncoder: StringDecoder: Tag]: JsonSchema[A] =
    stringSchema.tiemap(StringDecoder[A].decode, StringEncoder[A].encode)

  // --- Not Required ---

  implicit def optionSchema[A: Tag](implicit elem: JsonSchema[A]): JsonSchema[Option[A]] =
    NotRequired(Tag[Option[A]], JsonCodec.option[A](using elem.codec.encoder, elem.codec.decoder), elem, true, true)

  // --- Arr ---

  implicit def seqSchema[A: Tag](implicit elem: JsonSchema[A]): JsonSchema[Seq[A]] =
    JsonArr(Tag[Seq[A]], JsonCodec.seq[A](using elem.codec.encoder, elem.codec.decoder), elem)
  implicit def listSchema[A: Tag](implicit elem: JsonSchema[A]): JsonSchema[List[A]] =
    JsonArr(Tag[List[A]], JsonCodec.list[A](using elem.codec.encoder, elem.codec.decoder), elem)
  implicit def nelSchema[A: Tag](implicit elem: JsonSchema[A]): JsonSchema[NonEmptyList[A]] =
    JsonArr(Tag[NonEmptyList[A]], catsNelJsonCodec[A](using elem.codec), elem)

  // =====| Derived Instances |=====

  private object DerivingUtil {

    def genericLabelling[Annotated, Annotation](annotations: FieldAnnotations[Annotated, Annotation], labelling: Labelling[Annotated], f: Annotation => String): List[String] =
      annotations.annotationsList.zip(labelling.elemLabels).map { (a, l) => a.fold(l)(f) }

  }

  override inline implicit def genProduct[A](implicit m: K0.ProductGeneric[A]): Derived[JsonSchema[A]] = {
    val tag = Tag[A]
    val inst = K0.ProductInstances.of[A, JsonSchema]
    val labels = Labelling.of[A]
    val hint = OptionalAnnotation[A, jsonHint]
    val jsonFields = FieldAnnotations[A, jsonField]

    Derived(
      JsonSchema.ProductJsonObj(
        tag,
        DeriveJsonCodec.gen[A],
        hint.annotation.fold(labels.label)(_.name),
        Lazy {
          DerivingUtil
            .genericLabelling(jsonFields, labels, _.name)
            .zip(inst.instances)
            .map(ProductField(_, _))
        },
      ),
    )
  }

  override inline implicit def genSum[A](implicit m: K0.SumGeneric[A]): Derived[JsonSchema[A]] = {
    val tag = Tag[A]
    val inst = K0.SumInstances.of[A, JsonSchema].narrow[ProductJsonObj] // TODO (KR) : potentially relax this only for when there is a descriptor
    val descriptor = OptionalAnnotation[A, jsonDiscriminator]

    Derived(
      JsonSchema.SumJsonObj(
        tag,
        DeriveJsonCodec.gen[A],
        descriptor.annotation.map(_.name),
        inst.children.map(SumOption(_)),
      ),
    )
  }

}
