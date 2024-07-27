package harness.schema

import cats.syntax.option.*
import harness.core.*
import harness.deriving.ExampleK0Types.*
import harness.zio.*
import harness.zio.test.*
import zio.*
import zio.test.*
import zio.test.Assertion.*

object SchemaSpecsSpec extends DefaultHarnessSpec {

  enum MyEnum extends Enum[MyEnum] { case Value1, Value2, Value3 }
  object MyEnum extends Enum.Companion[MyEnum]

  final case class MyProduct(
      field1: String,
      field2: Option[MyEnum],
      field3: List[MyEnum],
  )

  implicit val productSimpleInstance: JsonSchema[ProductSimple] = JsonSchema.derived
  implicit val sumSimpleInstance: JsonSchema[SumSimple] = JsonSchema.derived
  implicit val selfRecursiveProductInstance: JsonSchema[SelfRecursiveProduct] = JsonSchema.derived
  implicit val otherRecursiveProduct1Instance: JsonSchema[OtherRecursiveProduct1] = JsonSchema.derived
  implicit val otherRecursiveProduct2Instance: JsonSchema[OtherRecursiveProduct2] = JsonSchema.derived
  implicit val selfRecursiveSumInstance: JsonSchema[SelfRecursiveSum] = JsonSchema.derived
  implicit val myProductInstance: JsonSchema[MyProduct] = JsonSchema.derived

  def showOverride: PartialFunction[Matchable, IndentedString] = {
    case tag: HTag[?] =>
      tag.prefixAll
    case SchemaSpec.Field(key, value) =>
      IndentedString.inline(
        IndentedString.StrWithJoin(s"$key:", " "),
        IndentedString.fromAny(value)(showOverride),
      )
  }

  private def makeTest(name: String)(schemas: Schema[?]*)(exp: PartialFunction[List[SchemaRef], List[SchemaSpec]]): TestSpec =
    test(name) {
      val specs = SchemaSpecs.fromSchemas(schemas.toList).specs.sorted

      exp.lift(specs.map(_.ref)) match {
        case Some(exp) => assert(specs)(equalTo(exp))
        case None      => throw new RuntimeException(s"Invalid schema-spec partial function provided:\n\n${IndentedString.inline(specs.map(IndentedString.fromAny(_)(showOverride)))}")
      }
    }

  override def testSpec: TestSpec =
    suite("SchemaSpecsSpec")(
      makeTest("MyProduct")(myProductInstance) { case List(myEnum, myEnumList, myEnumOption, myProduct, string) =>
        List(
          SchemaSpec(myEnum, SchemaSpec.JsonStr(List("Value1", "Value2", "Value3").some)),
          SchemaSpec(myEnumList, SchemaSpec.JsonArr(myEnum)),
          SchemaSpec(myEnumOption, SchemaSpec.JsonNotRequired(myEnum, true, true)),
          SchemaSpec(
            myProduct,
            SchemaSpec.JsonObj(
              List(
                SchemaSpec.Field("field1", SchemaSpec.FieldValue.Ref(string, None)),
                SchemaSpec.Field("field2", SchemaSpec.FieldValue.Ref(myEnumOption, myEnum.some)),
                SchemaSpec.Field("field3", SchemaSpec.FieldValue.Ref(myEnumList, None)),
              ),
            ),
          ),
          SchemaSpec(string, SchemaSpec.JsonStr(None)),
        )
      },
    )

}
