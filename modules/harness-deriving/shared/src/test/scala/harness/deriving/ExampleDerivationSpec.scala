package harness.deriving

import harness.core.*
import harness.deriving.ExampleTypes.*
import harness.zio.*
import harness.zio.test.*
import scala.deriving.Mirror.{ProductOf, SumOf}
import zio.test.*
import zio.test.Assertion.*

object ExampleDerivationSpec extends DefaultHarnessSpec {

  trait Show[A] {
    def show(a: A): IndentedString
  }
  object Show extends K0.Derivable[Show] {

    implicit def fromStringEncoder[A](implicit enc: StringEncoder[A]): Show[A] =
      enc.encode(_)

    implicit def option[A](implicit show: Show[A]): Show[Option[A]] = {
      case Some(a) => show.show(a)
      case None    => IndentedString.inline()
    }

    override inline implicit def genProduct[A](implicit m: ProductOf[A]): Derived[Show[A]] = {
      val labels = Labelling.of[A]
      val inst = K0.ProductInstances.of[A, Show]

      Derived { a =>
        inst
          .withInstance(a)
          .map
          .withLabels(labels) {
            [t] => (l: String, i: Show[t], b: t) => IndentedString.inline(s"$l:", IndentedString.indented(i.show(b)))
          }
      }
    }

    override inline implicit def genSum[A](implicit m: SumOf[A]): Derived[Show[A]] = {
      val labels = Labelling.of[A]
      val inst = K0.SumInstances.of[A, Show]

      Derived { a =>
        val wi = inst.withInstance(a)
        IndentedString.inline(
          s"${labels.elemLabels(wi.ord)}:",
          IndentedString.indented(wi.inst.show(a.asInstanceOf)),
        )
      }
    }

  }

  // =====| Instances |=====

  implicit val productSimpleInstance: Show[ProductSimple] = Show.derive
  implicit val sumSimpleInstance: Show[SumSimple] = Show.derive
  implicit val selfRecursiveProductInstance: Show[SelfRecursiveProduct] = Show.derive
  implicit val otherRecursiveProduct1Instance: Show[OtherRecursiveProduct1] = Show.derive
  implicit val otherRecursiveProduct2Instance: Show[OtherRecursiveProduct2] = Show.derive
  implicit val selfRecursiveSumInstance: Show[SelfRecursiveSum] = Show.derive

  // =====| Test |=====

  private def makeSuite[A](name: String)(show: Show[A], instances: List[A]): TestSpec =
    suite(name)(
      instances.zipWithIndex.map { case (inst, idx) =>
        test(s"Case ${idx + 1}") {
          val shown = show.show(inst)
          Logger.log.debug(shown).as(assertCompletes)
        }
      } *,
    )

  // override def logLevel: Logger.LogLevel = Logger.LogLevel.Debug

  override def spec: TestSpec =
    suite("ExampleDerivationSpec")(
      makeSuite("ProductSimple")(productSimpleInstance, ProductSimple.instances),
      makeSuite("SumSimple")(sumSimpleInstance, SumSimple.instances),
      makeSuite("SelfRecursiveProduct")(selfRecursiveProductInstance, SelfRecursiveProduct.instances),
      makeSuite("OtherRecursiveProduct1")(otherRecursiveProduct1Instance, OtherRecursiveProduct1.instances),
      makeSuite("OtherRecursiveProduct2")(otherRecursiveProduct2Instance, OtherRecursiveProduct2.instances),
      makeSuite("SelfRecursiveSum")(selfRecursiveSumInstance, SelfRecursiveSum.instances),
    )

}
