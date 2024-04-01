package harness.deriving

import cats.syntax.option.*

object ExampleK0Types {

  /*
  implicit val productSimpleInstance: MyTypeClass[ProductSimple] = MyTypeClass.derive
  implicit val sumSimpleInstance: MyTypeClass[SumSimple] = MyTypeClass.derive
  implicit val selfRecursiveProductInstance: MyTypeClass[SelfRecursiveProduct] = MyTypeClass.derive
  implicit val otherRecursiveProduct1Instance: MyTypeClass[OtherRecursiveProduct1] = MyTypeClass.derive
  implicit val otherRecursiveProduct2Instance: MyTypeClass[OtherRecursiveProduct2] = MyTypeClass.derive
  implicit val selfRecursiveSumInstance: MyTypeClass[SelfRecursiveSum] = MyTypeClass.derive
   */

  // =====| Simple |=====

  final case class ProductSimple(
      string: String,
      int: Int,
      boolean: Boolean,
      optString: Option[String],
      optInt: Option[Int],
      optBoolean: Option[Boolean],
  )
  object ProductSimple {
    val instances: List[ProductSimple] = List(
      ProductSimple("str", 1, true, None, None, None),
      ProductSimple("str", 1, true, "str2".some, 2.some, false.some),
    )
  }

  @zio.json.jsonDiscriminator("type")
  sealed trait SumSimple
  object SumSimple {

    final case class Case1(string: String, int: Int, boolean: Boolean) extends SumSimple
    object Case1 {
      val instances: List[Case1] = List(
        Case1("str", 1, true),
      )
    }

    final case class Case2(optString: Option[String], optInt: Option[Int], optBoolean: Option[Boolean]) extends SumSimple
    object Case2 {
      val instances: List[Case2] = List(
        Case2(None, None, None),
        Case2("str2".some, 2.some, false.some),
      )
    }

    final case class Case3(product: ProductSimple, timestamp: java.time.LocalDateTime) extends SumSimple
    object Case3 {
      val instances: List[Case3] = ProductSimple.instances.map(Case3(_, java.time.LocalDateTime.of(1, 1, 1, 1, 1)))
    }

    final case class Case4(optProduct: Option[ProductSimple]) extends SumSimple
    object Case4 {
      val instances: List[Case4] = Case4(None) :: ProductSimple.instances.map(i => Case4(i.some))
    }

    case object Case5 extends SumSimple

    val instances: List[SumSimple] = List(
      Case1.instances,
      Case2.instances,
      Case3.instances,
      Case4.instances,
      Case5 :: Nil,
    ).flatten

  }

  // =====| Self Recursive Product |=====

  final case class SelfRecursiveProduct(
      field: String,
      self: Option[SelfRecursiveProduct],
  )
  object SelfRecursiveProduct {
    val instances: List[SelfRecursiveProduct] = {
      val tmp1 = SelfRecursiveProduct("value1", None)
      val tmp2 = SelfRecursiveProduct("value2", tmp1.some)
      val tmp3 = SelfRecursiveProduct("value3", tmp2.some)
      List(tmp1, tmp2, tmp3)
    }
  }

  // =====| Other Recursive Product |=====

  private def tmp_otherRecursiveProduct1_1 = OtherRecursiveProduct1("value1", None)
  private def tmp_otherRecursiveProduct2_1 = OtherRecursiveProduct2("value1", None)

  private def tmp_otherRecursiveProduct1_2 = OtherRecursiveProduct1("value2", tmp_otherRecursiveProduct2_1.some)
  private def tmp_otherRecursiveProduct2_2 = OtherRecursiveProduct2("value2", tmp_otherRecursiveProduct1_1.some)

  private def tmp_otherRecursiveProduct1_3 = OtherRecursiveProduct1("value3", tmp_otherRecursiveProduct2_2.some)
  private def tmp_otherRecursiveProduct2_3 = OtherRecursiveProduct2("value3", tmp_otherRecursiveProduct1_2.some)

  final case class OtherRecursiveProduct1(
      product1Field: String,
      product2: Option[OtherRecursiveProduct2],
  )
  object OtherRecursiveProduct1 {
    val instances: List[OtherRecursiveProduct1] = List(
      tmp_otherRecursiveProduct1_1,
      tmp_otherRecursiveProduct1_2,
      tmp_otherRecursiveProduct1_3,
    )
  }

  final case class OtherRecursiveProduct2(
      product2Field: String,
      product1: Option[OtherRecursiveProduct1],
  )
  object OtherRecursiveProduct2 {

    val instances: List[OtherRecursiveProduct2] = List(
      tmp_otherRecursiveProduct2_1,
      tmp_otherRecursiveProduct2_2,
      tmp_otherRecursiveProduct2_3,
    )
  }

  // =====| Self Recursive Sum |=====

  sealed trait SelfRecursiveSum
  object SelfRecursiveSum {

    final case class Case1(string: String, int: Int, boolean: Boolean) extends SelfRecursiveSum
    object Case1 {
      val instances: List[Case1] = List(
        Case1("str", 1, true),
      )
    }

    final case class Case2(info: String, sum: SelfRecursiveSum) extends SelfRecursiveSum
    object Case2 {
      val instances: List[Case2] = List(
        Case1.instances.map(Case2("info-details", _)),
        Case2("info-details", Case3) :: Nil,
      ).flatten
    }

    case object Case3 extends SelfRecursiveSum

    val instances: List[SelfRecursiveSum] = List(
      Case1.instances,
      Case2.instances,
      Case3 :: Nil,
    ).flatten

  }

}
