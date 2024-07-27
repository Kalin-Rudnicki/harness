package harness.schema

import harness.schema.internal.TrimSchema
import java.util.UUID
import scala.annotation.tailrec

final case class SchemaSpecs private (specs: List[SchemaSpec], rewriteRef: SchemaSpecs.RefRewriteMap) {

  def ++(that: SchemaSpecs): SchemaSpecs = SchemaSpecs.deDuplicate(SchemaSpecs(this.specs ++ that.specs, this.rewriteRef ++ that.rewriteRef))

}
object SchemaSpecs {

  /**
    * Represents a mapping of UUID=>UUID.
    * No UUID which is present in the key-set should also be in the value-set.
    * This is because you would then require multiple recursions in order to get from old->new.
    */
  final case class RefRewriteMap(map: Map[UUID, UUID]) {

    def apply(schemaRef: SchemaRef): SchemaRef = map.get(schemaRef.id) match
      case Some(newId) => schemaRef.withId(newId)
      case None        => schemaRef

    def ++(that: RefRewriteMap): RefRewriteMap = {
      val newMapThis: Map[UUID, UUID] =
        this.map.map { case (k, v) => (k, that.map.getOrElse(v, v)) }
      val newMapThat: Map[UUID, UUID] =
        that.map.map { case (k, v) => (k, this.map.getOrElse(v, v)) }

      new RefRewriteMap(newMapThis ++ newMapThat)
    }

  }
  object RefRewriteMap {
    val empty: RefRewriteMap = new RefRewriteMap(Map.empty)
  }

  private def base(specs: List[SchemaSpec]): SchemaSpecs = new SchemaSpecs(specs, RefRewriteMap.empty)

  private def replaceIdForField(field: SchemaSpec.Field, idMap: RefRewriteMap): SchemaSpec.Field =
    SchemaSpec.Field(
      field.key,
      field.value match {
        case c: SchemaSpec.FieldValue.Const                   => c
        case SchemaSpec.FieldValue.Ref(fieldRef, requiredRef) => SchemaSpec.FieldValue.Ref(idMap(fieldRef), requiredRef.map(idMap(_)))
      },
    )

  private def replaceIdForDetails(details: SchemaSpec.Details, idMap: RefRewriteMap): SchemaSpec.Details =
    details match {
      case details: SchemaSpec.RawDetails =>
        details match {
          case SchemaSpec.RawStr(enumValues) => SchemaSpec.RawStr(enumValues)
          case SchemaSpec.RawJWT             => SchemaSpec.RawJWT
          case SchemaSpec.JWT(jsonRef)       => SchemaSpec.JWT(idMap(jsonRef))
        }
      case details: SchemaSpec.JsonDetails =>
        details match {
          case SchemaSpec.JsonNum                                           => SchemaSpec.JsonNum
          case SchemaSpec.JsonBool                                          => SchemaSpec.JsonBool
          case SchemaSpec.JsonStr(enumValues)                               => SchemaSpec.JsonStr(enumValues)
          case SchemaSpec.JsonNotRequired(elemRef, canBeNull, canBeMissing) => SchemaSpec.JsonNotRequired(idMap(elemRef), canBeNull, canBeMissing)
          case SchemaSpec.JsonArr(elemRef)                                  => SchemaSpec.JsonArr(idMap(elemRef))
          case SchemaSpec.JsonSum(optionRefs)                               => SchemaSpec.JsonSum(optionRefs.map(idMap(_)))
          case SchemaSpec.JsonObj(fields)                                   => SchemaSpec.JsonObj(fields.map(replaceIdForField(_, idMap)))
        }
    }

  private def replaceIdForSpec(spec: SchemaSpec, idMap: RefRewriteMap): SchemaSpec =
    SchemaSpec(idMap(spec.ref), replaceIdForDetails(spec.details, idMap))

  private def step(specs: SchemaSpecs): SchemaSpecs = {
    val newlyMappedIds: Map[UUID, UUID] =
      specs.specs.groupMap(s => (s.ref.tag, s.details))(_.ref.id).toList.flatMap { case (_, id0 :: idN) => idN.map(_ -> id0); case _ => Nil }.toMap
    val newIdMap: RefRewriteMap =
      specs.rewriteRef ++ RefRewriteMap(newlyMappedIds)
    val newSpecs: List[SchemaSpec] =
      specs.specs.flatMap { spec =>
        Option.when(!newIdMap.map.contains(spec.ref.id)) { replaceIdForSpec(spec, newIdMap) }
      }

    SchemaSpecs(newSpecs, newIdMap)
  }

  @tailrec
  private def deDuplicate(specs: SchemaSpecs): SchemaSpecs = {
    val afterStep: SchemaSpecs = step(specs)
    if (specs.rewriteRef == afterStep.rewriteRef) specs
    else deDuplicate(afterStep)
  }

  def fromSchemas(schemas: List[Schema[?]]): SchemaSpecs =
    deDuplicate(base(TrimSchema.convert(schemas).flatMap(SchemaSpec.fromTrimmed)))

  def fromSchemas(schemas: Schema[?]*): SchemaSpecs =
    fromSchemas(schemas.toList)

}
