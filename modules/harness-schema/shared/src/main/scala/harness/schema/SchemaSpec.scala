package harness.schema

import cats.syntax.option.*
import harness.schema.internal.*

final case class SchemaSpec(
    ref: SchemaRef,
    details: SchemaSpec.Details,
) {

  val baseType: String =
    details match {
      case _: SchemaSpec.JsonDetails => "json"
      case _: SchemaSpec.RawDetails  => "raw"
    }

}
object SchemaSpec {

  implicit val ordering: Ordering[SchemaSpec] = Ordering.by(_.ref)

  sealed trait Details

  // =====| Raw |=====

  sealed trait RawDetails extends Details

  final case class RawStr(enumValues: Option[List[String]]) extends RawDetails

  case object RawJWT extends RawDetails

  final case class JWT(jsonRef: SchemaRef) extends RawDetails

  // =====| Json |=====

  sealed trait JsonDetails extends Details

  case object JsonNum extends JsonDetails
  case object JsonBool extends JsonDetails
  final case class JsonStr(enumValues: Option[List[String]]) extends JsonDetails

  final case class JsonNotRequired(
      elemRef: SchemaRef,
      canBeNull: Boolean,
      canBeMissing: Boolean,
  ) extends JsonDetails

  final case class JsonArr(elemRef: SchemaRef) extends JsonDetails

  sealed trait FieldValue
  object FieldValue {
    final case class Ref(fieldRef: SchemaRef, requiredRef: Option[SchemaRef]) extends FieldValue
    final case class Const(const: String) extends FieldValue
  }

  final case class Field(key: String, value: FieldValue)

  final case class JsonObj(fields: List[Field]) extends JsonDetails

  final case class JsonSum(optionRefs: List[SchemaRef]) extends JsonDetails

  // =====| Conversion |=====

  private[schema] def fromTrimmed(schema: TrimmedSchema): List[SchemaSpec] =
    schema match {
      case schema: TrimmedJsonSchema =>
        schema match {
          case TrimmedJsonSchema.Duplicate(_)                                    => Nil
          case TrimmedJsonSchema.JsonNum(ref)                                    => SchemaSpec(ref, JsonNum) :: Nil
          case TrimmedJsonSchema.JsonBool(ref)                                   => SchemaSpec(ref, JsonBool) :: Nil
          case TrimmedJsonSchema.JsonStr(ref, enumValues)                        => SchemaSpec(ref, JsonStr(enumValues)) :: Nil
          case TrimmedJsonSchema.NotRequired(ref, elem, canBeNull, canBeMissing) => SchemaSpec(ref, JsonNotRequired(elem.ref, canBeNull, canBeMissing)) :: fromTrimmed(elem)
          case TrimmedJsonSchema.Arr(ref, elem)                                  => SchemaSpec(ref, JsonArr(elem.ref)) :: fromTrimmed(elem)
          case TrimmedJsonSchema.Obj(ref, fields) =>
            val fs = fields.map {
              case TrimmedJsonSchema.Field(key, TrimmedJsonSchema.FieldValue.Value(value)) =>
                val req = value match {
                  case TrimmedJsonSchema.NotRequired(_, elem, _, _) => elem.ref.some
                  case _                                            => None
                }
                Field(key, FieldValue.Ref(value.ref, req))
              case TrimmedJsonSchema.Field(key, TrimmedJsonSchema.FieldValue.Const(const)) =>
                Field(key, FieldValue.Const(const))
            }
            SchemaSpec(ref, JsonObj(fs)) :: fields.flatMap(_.value.optElem).flatMap(fromTrimmed)
          case TrimmedJsonSchema.Sum(ref, options) =>
            SchemaSpec(ref, JsonSum(options.map(_.ref))) :: options.flatMap(fromTrimmed)
        }
      case schema: TrimmedRawSchema =>
        schema match {
          case TrimmedRawSchema.Duplicate(_)                => Nil
          case TrimmedRawSchema.Str(ref, enumValues)        => SchemaSpec(ref, RawStr(enumValues)) :: Nil
          case TrimmedRawSchema.RawJWT(ref)                 => SchemaSpec(ref, RawJWT) :: Nil
          case TrimmedRawSchema.JWT(ref, trimmedJsonSchema) => SchemaSpec(ref, JWT(trimmedJsonSchema.ref)) :: fromTrimmed(trimmedJsonSchema)
        }
    }

}
