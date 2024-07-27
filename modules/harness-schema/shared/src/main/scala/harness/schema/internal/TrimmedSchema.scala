package harness.schema.internal

import cats.syntax.option.*
import harness.schema.*
import java.util.UUID

sealed trait TrimmedSchema {
  val ref: SchemaRef
  val ids: Set[UUID]
}

// =====| Raw |=====

sealed trait TrimmedRawSchema extends TrimmedSchema
object TrimmedRawSchema {

  final case class Duplicate(ref: SchemaRef) extends TrimmedRawSchema {
    override val ids: Set[UUID] = Set(ref.id)
  }

  final case class Str(ref: SchemaRef, enumValues: Option[List[String]]) extends TrimmedRawSchema {
    override val ids: Set[UUID] = Set(ref.id)
  }

  final case class RawJWT(ref: SchemaRef) extends TrimmedRawSchema {
    override val ids: Set[UUID] = Set(ref.id)
  }


  final case class JWT(ref: SchemaRef, trimmedJsonSchema: TrimmedJsonSchema) extends TrimmedRawSchema {
    override val ids: Set[UUID] = trimmedJsonSchema.ids + ref.id
  }
  
}

// =====| Json |=====

sealed trait TrimmedJsonSchema extends TrimmedSchema
object TrimmedJsonSchema {

  sealed trait Single extends TrimmedJsonSchema {
    override final val ids: Set[UUID] = Set(ref.id)
  }

  final case class Duplicate(ref: SchemaRef) extends TrimmedJsonSchema.Single

  final case class JsonNum(ref: SchemaRef) extends TrimmedJsonSchema.Single
  final case class JsonBool(ref: SchemaRef) extends TrimmedJsonSchema.Single
  final case class JsonStr(ref: SchemaRef, enumValues: Option[List[String]]) extends TrimmedJsonSchema.Single

  final case class NotRequired(
      ref: SchemaRef,
      elem: TrimmedJsonSchema,
      canBeNull: Boolean,
      canBeMissing: Boolean,
  ) extends TrimmedJsonSchema {
    override val ids: Set[UUID] = elem.ids + ref.id
  }

  final case class Arr(ref: SchemaRef, elem: TrimmedJsonSchema) extends TrimmedJsonSchema {
    override val ids: Set[UUID] = elem.ids + ref.id
  }

  sealed trait FieldValue {
    final def optElem: Option[TrimmedJsonSchema] =
      this match {
        case FieldValue.Value(elem) => elem.some
        case FieldValue.Const(_)    => None
      }
  }
  object FieldValue {
    final case class Value(elem: TrimmedJsonSchema) extends FieldValue
    final case class Const(const: String) extends FieldValue
  }

  final case class Field(key: String, value: FieldValue)

  final case class Obj(ref: SchemaRef, fields: List[Field]) extends TrimmedJsonSchema {
    override val ids: Set[UUID] = fields.flatMap(_.value.optElem).flatMap(_.ids).toSet + ref.id
  }

  final case class Sum(ref: SchemaRef, options: List[TrimmedJsonSchema]) extends TrimmedJsonSchema {
    override val ids: Set[UUID] = options.flatMap(_.ids).toSet + ref.id
  }

}
