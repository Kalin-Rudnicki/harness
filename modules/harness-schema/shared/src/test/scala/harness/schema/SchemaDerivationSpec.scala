package harness.schema

import harness.core.*
import harness.deriving.ExampleTypes.*
import harness.schema.internal.*
import harness.zio.*
import harness.zio.test.*
import zio.test.*
import zio.test.Assertion.*

object SchemaDerivationSpec extends DefaultHarnessSpec {

  @scala.annotation.unused
  private def show(schema: Schema[?]): IndentedString =
    schema match {
      case schema: JsonSchema[?] =>
        schema match {
          case JsonSchema.JsonNum(tag, _) =>
            IndentedString.inline(
              s"type: ${TypeOps.typeName(true, TypeOps.PackagePrefix.All)(using tag)}",
              s"json-type: number",
            )
          case JsonSchema.JsonBool(tag, _) =>
            IndentedString.inline(
              s"type: ${TypeOps.typeName(true, TypeOps.PackagePrefix.All)(using tag)}",
              s"json-type: boolean",
            )
          case JsonSchema.JsonStr(tag, _, enumValues) =>
            IndentedString.inline(
              s"type: ${TypeOps.typeName(true, TypeOps.PackagePrefix.All)(using tag)}",
              s"json-type: string",
              enumValues.map { values =>
                IndentedString.inline(
                  "enums:",
                  IndentedString.indented(
                    values.map(v => s"- $v"),
                  ),
                )
              },
            )
          case JsonSchema.NotRequired(tag, _, elem, canBeNull, canBeMissing) =>
            IndentedString.inline(
              s"type: not-required", // TODO (KR) :
              s"json-type: TODO", // TODO (KR) :
              s"can-be-null: $canBeNull",
              s"can-be-missing: $canBeMissing",
              "elem:",
              IndentedString.indented(show(elem)),
            )
          case JsonSchema.JsonArr(tag, _, elem) =>
            IndentedString.inline(
              s"type: ${TypeOps.typeName(true, TypeOps.PackagePrefix.All)(using tag)}",
              s"json-type: array",
              "elem:",
              IndentedString.indented(show(elem)),
            )
          case JsonSchema.ProductJsonObj(tag, _, _, elems) =>
            IndentedString.inline(
              s"type: ${TypeOps.typeName(true, TypeOps.PackagePrefix.All)(using tag)}",
              s"json-type: object",
              s"elems:",
              IndentedString.indented(
                elems.value.map { case JsonSchema.ProductField(key, elem) =>
                  IndentedString.inline(
                    s"$key:",
                    IndentedString.indented(show(elem)),
                  )
                },
              ),
            )
          case JsonSchema.SumJsonObj(tag, _, discriminator, options) =>
            IndentedString.inline(
              s"type: ${TypeOps.typeName(true, TypeOps.PackagePrefix.All)(using tag)}",
              s"json-type: object",
              discriminator.map(d => s"discriminator: $d"),
              s"options:",
              IndentedString.indented(
                options.map { opt =>
                  IndentedString.inline(
                    s"${opt.option.name}:",
                    IndentedString.indented(show(opt.option)),
                  )
                },
              ),
            )
        }
      case schema: RawSchema[?] =>
        schema match {
          case RawSchema.Str(tag, _, enumValues) =>
            IndentedString.inline(
              s"type: ${TypeOps.typeName(true, TypeOps.PackagePrefix.All)(using tag)}",
              "raw-type: string",
              enumValues.map { values =>
                IndentedString.inline(
                  "enums:",
                  IndentedString.indented(
                    values.map(v => s"- $v"),
                  ),
                )
              },
            )
        }
    }

  extension (self: SchemaRef) {
    def show: String = s"${TypeOps.typeName(true, TypeOps.PackagePrefix.All)(using self.tag)} (${self.id})"
  }

  private def showTrimmed(schema: TrimmedSchema): IndentedString =
    schema match {
      case schema: TrimmedJsonSchema =>
        schema match {
          case TrimmedJsonSchema.Duplicate(ref) =>
            s"Duplicate: ${ref.show}"
          case TrimmedJsonSchema.JsonNum(ref) =>
            s"Number: ${ref.show}"
          case TrimmedJsonSchema.JsonBool(ref) =>
            s"Boolean: ${ref.show}"
          case TrimmedJsonSchema.JsonStr(ref, enumValues) =>
            IndentedString.inline(
              s"Str: ${ref.show}",
              enumValues.map { values =>
                IndentedString.indented(
                  "enum-values:",
                  IndentedString.Indented(
                    values.map(v => s"- $v"),
                  ),
                )
              },
            )
          case TrimmedJsonSchema.NotRequired(ref, elem, canBeNull, canBeMissing) =>
            IndentedString.inline(
              s"one-of: (${ref.show})",
              IndentedString.indented(
                Option.when(canBeNull)("- null"),
                Option.when(canBeMissing)("- missing"),
                "- value:",
                IndentedString.indented(showTrimmed(elem)),
              ),
            )
          case TrimmedJsonSchema.Arr(ref, elem) =>
            IndentedString.inline(
              s"array: ${ref.show}",
              IndentedString.indented(showTrimmed(elem)),
            )
          case TrimmedJsonSchema.Obj(ref, fields) =>
            IndentedString.inline(
              s"object: ${ref.show}",
              IndentedString.indented(
                fields.map[IndentedString] { field =>
                  field.value match {
                    case TrimmedJsonSchema.FieldValue.Value(elem) =>
                      IndentedString.inline(
                        s"- ${field.key}:",
                        IndentedString.indented(showTrimmed(elem)),
                      )
                    case TrimmedJsonSchema.FieldValue.Const(const) =>
                      s"- ${field.key}: const($const)"
                  }
                },
              ),
            )
          case TrimmedJsonSchema.Sum(ref, options) =>
            IndentedString.inline(
              s"one-of:: ${ref.show}",
              IndentedString.indented(
                options.map { option =>
                  IndentedString.inline(
                    "-:",
                    IndentedString.indented(showTrimmed(option)),
                  )
                },
              ),
            )
        }
      case schema: TrimmedRawSchema =>
        schema match {
          case TrimmedRawSchema.Duplicate(ref) =>
            s"Duplicate: ${ref.show}"
          case TrimmedRawSchema.Str(ref, enumValues) =>
            IndentedString.inline(
              s"Raw Str: ${ref.show}",
              enumValues.map { values =>
                IndentedString.indented(
                  "enum-values:",
                  IndentedString.Indented(
                    values.map(v => s"- $v"),
                  ),
                )
              },
            )
        }
    }

  @scala.annotation.unused
  private def showSpec(spec: SchemaSpec): IndentedString =
    IndentedString.inline(
      s"--- ${spec.ref.show} <${spec.baseType}> ---",
      spec.details match {
        case details: SchemaSpec.JsonDetails =>
          details match {
            case SchemaSpec.JsonNum =>
              "type: number"
            case SchemaSpec.JsonBool =>
              "type: boolean"
            case SchemaSpec.JsonStr(enumValues) =>
              IndentedString.inline(
                "type: string",
                enumValues.map { values =>
                  IndentedString.inline(
                    "enum-values:",
                    IndentedString.indented(values.map { v => s"- $v" }),
                  )
                },
              )
            case SchemaSpec.JsonNotRequired(elemRef, canBeNull, canBeMissing) =>
              IndentedString.inline(
                "type: not-required",
                s"ref: ${elemRef.show}",
                s"can-be-null: $canBeNull",
                s"can-be-missing: $canBeMissing",
              )
            case SchemaSpec.JsonArr(elemRef) =>
              IndentedString.inline(
                "type: array",
                s"elem: ${elemRef.show}",
              )
            case SchemaSpec.JsonObj(fields) =>
              IndentedString.inline(
                "type: object",
                "fields:",
                IndentedString.indented(
                  fields.map[IndentedString] {
                    case SchemaSpec.Field(key, SchemaSpec.FieldValue.Ref(fieldRef, req)) =>
                      IndentedString.inline(
                        s"- $key: ${fieldRef.show}",
                        s"  required: ${req.isEmpty}",
                        req.map(r => s"  required-type: ${r.show}"),
                      )
                    case SchemaSpec.Field(key, SchemaSpec.FieldValue.Const(const)) => s"- $key: const($const)"
                  },
                ),
              )
            case SchemaSpec.JsonSum(optionRefs) =>
              IndentedString.inline(
                "type: sum",
                "options:",
                IndentedString.indented(
                  optionRefs.map { o => s"- ${o.show}" },
                ),
              )
          }
        case details: SchemaSpec.RawDetails =>
          details match {
            case SchemaSpec.RawStr(enumValues) =>
              IndentedString.inline(
                "type: raw-string",
                enumValues.map { values =>
                  IndentedString.indented(
                    "enum-values:",
                    IndentedString.Indented(
                      values.map(v => s"- $v"),
                    ),
                  )
                },
              )
          }
      },
    )

  // =====| Instances |=====

  implicit val productSimpleInstance: JsonSchema[ProductSimple] = JsonSchema.derive
  implicit val sumSimpleInstance: JsonSchema[SumSimple] = JsonSchema.derive
  implicit val selfRecursiveProductInstance: JsonSchema[SelfRecursiveProduct] = JsonSchema.derive
  implicit val otherRecursiveProduct1Instance: JsonSchema[OtherRecursiveProduct1] = JsonSchema.derive
  implicit val otherRecursiveProduct2Instance: JsonSchema[OtherRecursiveProduct2] = JsonSchema.derive
  implicit val selfRecursiveSumInstance: JsonSchema[SelfRecursiveSum] = JsonSchema.derive

  // ---  ---

  val specs: List[SchemaSpec] = SchemaSpec.fromSchemas(
    productSimpleInstance,
    sumSimpleInstance,
    selfRecursiveProductInstance,
    otherRecursiveProduct1Instance,
    otherRecursiveProduct2Instance,
    selfRecursiveSumInstance,
    RawSchema.encodedStringSchema[java.time.LocalTime],
  )

  /*
  println(
    IndentedString.inline(
      specs.sorted.map { s =>
        IndentedString.inline(
          IndentedString.Break,
          showSpec(s),
        )
      },
      IndentedString.Break,
    ),
  )
   */

  // =====| Tests |=====

  private def makeSpec[A](name: String)(schema: Schema[A]): TestSpec =
    test(name) {
      Logger.log.debug(showTrimmed(TrimSchema.convert(schema :: Nil).head)).as(assertCompletes)
    }

  // override def logLevel: Logger.LogLevel = Logger.LogLevel.Debug

  override def spec: TestSpec =
    suite("SchemaDerivationSpec")(
      makeSpec("ProductSimple")(productSimpleInstance),
      makeSpec("SumSimple")(sumSimpleInstance),
      makeSpec("SelfRecursiveProduct")(selfRecursiveProductInstance),
      makeSpec("OtherRecursiveProduct1")(otherRecursiveProduct1Instance),
      makeSpec("OtherRecursiveProduct2")(otherRecursiveProduct2Instance),
      makeSpec("SelfRecursiveSum")(selfRecursiveSumInstance),
    )

}
