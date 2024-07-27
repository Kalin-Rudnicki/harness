package harness.schema.internal

import harness.schema.*
import java.util.UUID
import scala.annotation.tailrec

object TrimSchema {

  def convert(schemas: List[Schema[?]]): List[TrimmedSchema] = {
    @tailrec
    def loop(ids: Set[UUID], queue: List[Schema[?]], rStack: List[TrimmedSchema]): List[TrimmedSchema] =
      queue match {
        case qHead :: qTail =>
          val converted = convert(ids, qHead)
          loop(ids | converted.ids, qTail, converted :: rStack)
        case Nil =>
          rStack.reverse
      }

    loop(Set.empty, schemas, Nil)
  }

  private def convert(ids: Set[UUID], schema: Schema[?]): TrimmedSchema =
    schema match {
      case schema: JsonSchema[?] => convertJson(ids, schema)
      case schema: RawSchema[?]  => convertRaw(ids, schema)
    }

  private def convertRaw(ids: Set[UUID], schema: RawSchema[?]): TrimmedRawSchema =
    if (ids.contains(schema.ref.id)) TrimmedRawSchema.Duplicate(schema.ref)
    else convertRawNew(ids, schema)

  private def convertRawNew(@scala.annotation.unused ids: Set[UUID], schema: RawSchema[?]): TrimmedRawSchema = schema match
    case RawSchema.Str(_, _, enumValues) => TrimmedRawSchema.Str(schema.ref, enumValues)
    case RawSchema.RawJWT(_, _)          => TrimmedRawSchema.RawJWT(schema.ref)
    case RawSchema.JWT(_, _, jsonSchema) => TrimmedRawSchema.JWT(schema.ref, convertJson(ids + schema.ref.id, jsonSchema))

  private def convertJson(ids: Set[UUID], schema: JsonSchema[?]): TrimmedJsonSchema =
    if (ids.contains(schema.ref.id)) TrimmedJsonSchema.Duplicate(schema.ref)
    else convertJsonNew(ids, schema)

  private def convertProductJson(ids: Set[UUID], schema: JsonSchema.ProductJsonObj[?]): TrimmedJsonSchema.Obj =
    TrimmedJsonSchema.Obj(
      schema.ref,
      schema.elems.value
        .foldLeft((ids + schema.ref.id, List.empty[TrimmedJsonSchema.Field])) { case ((ids, acc), JsonSchema.ProductField(name, schema)) =>
          val elem = convertJson(ids, schema)
          val field = TrimmedJsonSchema.Field(name, TrimmedJsonSchema.FieldValue.Value(elem))
          (
            ids | elem.ids,
            field :: acc,
          )
        }
        ._2
        .reverse,
    )

  private def convertSumJson(ids: Set[UUID], discriminator: Option[String], option: JsonSchema.SumOption[?]): TrimmedJsonSchema.Obj = {
    val base: TrimmedJsonSchema.Obj = convertProductJson(ids + option.ref.id, option.option)
    TrimmedJsonSchema.Obj(
      option.ref,
      discriminator match {
        case Some(discriminator) =>
          TrimmedJsonSchema.Field(discriminator, TrimmedJsonSchema.FieldValue.Const(option.option.name)) :: base.fields
        case None =>
          TrimmedJsonSchema.Field(
            option.option.name,
            TrimmedJsonSchema.FieldValue.Value(base),
          ) :: Nil
      },
    )
  }

  private def convertJsonNew(ids: Set[UUID], schema: JsonSchema[?]): TrimmedJsonSchema =
    schema match {
      case JsonSchema.JsonNum(_, _)             => TrimmedJsonSchema.JsonNum(schema.ref)
      case JsonSchema.JsonBool(_, _)            => TrimmedJsonSchema.JsonBool(schema.ref)
      case JsonSchema.JsonStr(_, _, enumValues) => TrimmedJsonSchema.JsonStr(schema.ref, enumValues)
      case JsonSchema.NotRequired(_, _, elem, canBeNull, canBeMissing) =>
        TrimmedJsonSchema.NotRequired(schema.ref, convertJson(ids + schema.ref.id, elem), canBeNull, canBeMissing)
      case JsonSchema.JsonArr(_, _, elem) =>
        TrimmedJsonSchema.Arr(schema.ref, convertJson(ids + schema.ref.id, elem))
      case schema: JsonSchema.ProductJsonObj[?] =>
        convertProductJson(ids, schema)
      case JsonSchema.SumJsonObj(_, _, discriminator, options) =>
        TrimmedJsonSchema.Sum(
          schema.ref,
          options
            .foldLeft((ids + schema.ref.id, List.empty[TrimmedJsonSchema])) { case ((ids, acc), option) =>
              val modified = convertSumJson(ids, discriminator, option)
              (
                ids | modified.ids,
                modified :: acc,
              )
            }
            ._2
            .reverse,
        )
    }

}
