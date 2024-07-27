package harness.deriving

import harness.core.*
import harness.deriving.ExampleK0Types.*
import harness.zio.*
import harness.zio.test.*
import scala.deriving.Mirror.{ProductOf, SumOf}
import zio.test.*
import zio.test.Assertion.*

object ExampleK0DerivationSpec extends DefaultHarnessSpec {

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
      val instances = K0.ProductInstances.of[A, Show]

      Derived { a =>
        instances
          .withInstance(a)
          .map
          .withLabels(labels) {
            [t] => (l: String, i: Show[t], b: t) => IndentedString.inline(s"$l:", IndentedString.indented(i.show(b)))
          }
      }
    }

    override inline implicit def genSum[A](implicit m: SumOf[A]): Derived[Show[A]] = {
      val labels = Labelling.of[A]
      val instances = K0.SumInstances.of[A, Show]

      Derived { a =>
        instances.withInstance(a).use.withLabels(labels) {
          [t <: A] =>
            (l: String, i: Show[t], t: t) =>
              IndentedString.inline(
                s"$l:",
                IndentedString.indented(i.show(t)),
            )
        }

      }
    }

  }

  // =====| Instances |=====

  implicit val productSimpleInstance: Show[ProductSimple] = Show.derived
  implicit val sumSimpleInstance: Show[SumSimple] = Show.derived
  implicit val selfRecursiveProductInstance: Show[SelfRecursiveProduct] = Show.derived
  implicit val otherRecursiveProduct1Instance: Show[OtherRecursiveProduct1] = Show.derived
  implicit val otherRecursiveProduct2Instance: Show[OtherRecursiveProduct2] = Show.derived
  implicit val selfRecursiveSumInstance: Show[SelfRecursiveSum] = Show.derived

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

  override def testSpec: TestSpec =
    suite("ExampleK0DerivationSpec")(
      makeSuite("ProductSimple")(productSimpleInstance, ProductSimple.instances),
      makeSuite("SumSimple")(sumSimpleInstance, SumSimple.instances),
      makeSuite("SelfRecursiveProduct")(selfRecursiveProductInstance, SelfRecursiveProduct.instances),
      makeSuite("OtherRecursiveProduct1")(otherRecursiveProduct1Instance, OtherRecursiveProduct1.instances),
      makeSuite("OtherRecursiveProduct2")(otherRecursiveProduct2Instance, OtherRecursiveProduct2.instances),
      makeSuite("SelfRecursiveSum")(selfRecursiveSumInstance, SelfRecursiveSum.instances),
    )

}
