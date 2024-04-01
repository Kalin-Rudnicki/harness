package harness.deriving

import cats.syntax.option.*
import harness.deriving.ExampleK11Types.*
import harness.zio.*
import harness.zio.test.*
import zio.test.*

object ExampleK11DerivationSpec extends DefaultHarnessSpec {

  import K11.~>

  trait FunctorK[F[_[_]]] {
    def mapK[A[_], B[_]](a: F[A])(f: A ~> B): F[B]
  }
  object FunctorK extends K11.Derivable[FunctorK] {

    implicit def id[X]: FunctorK[K11.Id[X]] =
      new FunctorK[K11.Id[X]] {
        override def mapK[A[_], B[_]](a: K11.Id[X][A])(f: A ~> B): K11.Id[X][B] = f(a)
      }

    override inline implicit def genProduct[F[_[_]]](implicit m: K11.ProductGeneric[F]): Derived[FunctorK[F]] = {
      val inst = K11.ProductInstances.of[F, FunctorK]

      Derived {
        new FunctorK[F] {
          override def mapK[A[_], B[_]](a: F[A])(f: A ~> B): F[B] =
            inst.withInstance(a).mapInstantiate { [t[_[_]]] => (i: FunctorK[t], t: t[A]) => i.mapK(t)(f) }
        }
      }
    }

    override inline implicit def genSum[F[_[_]]](implicit m: K11.SumGeneric[F]): Derived[FunctorK[F]] = {
      val inst = K11.SumInstances.of[F, FunctorK]

      Derived {
        new FunctorK[F] {
          override def mapK[A[_], B[_]](a: F[A])(f: A ~> B): F[B] =
            inst.withInstance(a).inst.mapK(a)(f)
        }
      }
    }

  }

  // =====| Instances |=====

  implicit val productSimpleFunctor: FunctorK[ProductSimple] = FunctorK.derive
  implicit val sumSimpleFunctor: FunctorK[SumSimple] = FunctorK.derive

  // =====| Test |=====

  // override def logLevel: Logger.LogLevel = Logger.LogLevel.Debug

  override def spec: TestSpec =
    suite("ExampleK11DerivationSpec")(
      suite("ProductSimple")(
        test("works-1") {
          val input = ProductSimple[K11.Identity](
            string = "string",
            int = 1,
            boolean = true,
            optString = "string2".some,
            optInt = 2.some,
            optBoolean = false.some,
          )
          val mapped = productSimpleFunctor.mapK[K11.Identity, Option](input) { [t] => (a: K11.Identity[t]) => a.some }
          val exp = ProductSimple[Option](
            string = "string".some,
            int = 1.some,
            boolean = true.some,
            optString = "string2".some.some,
            optInt = 2.some.some,
            optBoolean = false.some.some,
          )

          assertTrue(mapped == exp)
        },
      ),
    )

}
