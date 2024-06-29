package harness.deriving

import cats.syntax.option.*
import harness.deriving.ExampleK1Types.*
import harness.zio.*
import harness.zio.test.*
import zio.test.*

object ExampleK1DerivationSpec extends DefaultHarnessSpec {

  trait Functor[F[_]] {
    def map[A, B](a: F[A])(f: A => B): F[B]
  }
  object Functor extends K1.Derivable[Functor] {

    implicit val idFunctor: Functor[K1.Id] =
      new Functor[K1.Id] {
        override def map[A, B](a: K1.Id[A])(f: A => B): K1.Id[B] = f(a)
      }

    implicit val optionFunctor: Functor[Option] =
      new Functor[Option] {
        override def map[A, B](a: Option[A])(f: A => B): Option[B] = a.map(f)
      }

    implicit val listFunctor: Functor[List] =
      new Functor[List] {
        override def map[A, B](a: List[A])(f: A => B): List[B] = a.map(f)
      }

    override inline implicit def genProduct[F[_]](implicit m: K1.ProductGeneric[F]): Derived[Functor[F]] = {
      val instances = K1.ProductInstances.of[F, Functor]

      Derived {
        new Functor[F] {
          override def map[A, B](a: F[A])(f: A => B): F[B] =
            instances.withInstance(a).mapInstantiate {
              [t[_]] => (i: Functor[t], t: t[A]) => i.map(t)(f)
            }
        }
      }
    }

    override inline implicit def genSum[F[_]](implicit m: K1.SumGeneric[F]): Derived[Functor[F]] = {
      val instances = K1.SumInstances.of[F, Functor]

      Derived {
        new Functor[F] {
          override def map[A, B](a: F[A])(f: A => B): F[B] =
            instances.withInstance(a).use { [t[C] <: F[C]] => (i: Functor[t], t: t[A]) => i.map(t)(f) }
        }
      }
    }

  }

  // =====| Instances |=====

  implicit val productSimpleFunctor: Functor[ProductSimple] = Functor.derive
  implicit val sumSimpleFunctor: Functor[SumSimple] = Functor.derive

  // =====| Test |=====

  override def testSpec: TestSpec =
    suite("ExampleK1DerivationSpec")(
      suite("ProductSimple")(
        test("works-1") {
          val input = ProductSimple[String](
            option = "option".some,
            list = List("list1", "list2"),
          )
          val mapped = productSimpleFunctor.map(input)(s => s"$s-$s")
          val exp = ProductSimple[String](
            option = "option-option".some,
            list = List("list1-list1", "list2-list2"),
          )
          assertTrue(mapped == exp)
        },
        test("works-2") {
          val input = ProductSimple[String](
            option = None,
            list = List("list1", "list2"),
          )
          val mapped = productSimpleFunctor.map(input)(s => s"[$s]")
          val exp = ProductSimple[String](
            option = None,
            list = List("[list1]", "[list2]"),
          )
          assertTrue(mapped == exp)
        },
      ),
      suite("SumSimple")(
        test("works-1") {
          val input = SumSimple.Case1[String](
            option = "option".some,
          )
          val mapped = sumSimpleFunctor.map(input)(s => s"$s-$s")
          val exp = SumSimple.Case1[String](
            option = "option-option".some,
          )
          assertTrue(mapped == exp)
        },
      ),
    )

}
