package harness.deriving

import cats.syntax.option.*
import harness.zio.test.*
import scala.reflect.ClassTag
import zio.test.*

object UnionAndIntersectionSpec extends DefaultHarnessSpec {

  trait A { val a: Int }
  object A {

    final case class Inst(a: Int) extends A {
      override def productPrefix: String = "A"
    }

    def apply(a: Int): A = Inst(a)

  }

  trait B { val b: Int }
  object B {

    final case class Inst(b: Int) extends B {
      override def productPrefix: String = "B"
    }

    def apply(b: Int): B = Inst(b)

  }

  trait C { val c: Int }
  object C {

    final case class Inst(c: Int) extends C {
      override def productPrefix: String = "C"
    }

    def apply(c: Int): C = Inst(c)

  }

  trait D { val d: Int }
  object D {

    final case class Inst(d: Int) extends D {
      override def productPrefix: String = "D"
    }

    def apply(d: Int): D = Inst(d)

  }

  final case class AB(a: Int, b: Int) extends A with B
  final case class ABC(a: Int, b: Int, c: Int) extends A with B with C

  final case class MyTypeClass[T](
      options: List[T],
      eq: (T, T) => Boolean,
      cast: Any => Option[T],
  )
  object MyTypeClass extends K0.DerivableUnion.Auto[MyTypeClass] with K0.DerivableIntersection.Auto[MyTypeClass] {

    def apply[T](implicit ev: MyTypeClass[T]): MyTypeClass[T] = ev

    def make[T](options: T*)(implicit ev: ClassTag[T]): MyTypeClass[T] =
      MyTypeClass(
        options.toList,
        (a, b) => (a == b) && (b == a),
        _.asInstanceOf[Matchable] match {
          case ev(t) => t.some
          case _     => None
        },
      )

    override protected inline def foldUnion[A, B](ta: MyTypeClass[A], tb: MyTypeClass[B]): MyTypeClass[A | B] =
      MyTypeClass(
        ta.options ::: tb.options,
        { (a, b) =>
          def eval(mtc: MyTypeClass[?]): Option[Boolean] =
            (mtc.cast(a), mtc.cast(b)) match {
              case (Some(a), Some(b)) => mtc.eq(a, b).some
              case (None, None)       => None
              case _                  => false.some
            }

          val res = List(eval(ta), eval(tb)).flatten
          res.nonEmpty && !res.contains(false)
        },
        any => ta.cast(any).orElse(tb.cast(any)),
      )

    override protected inline def foldIntersection[A, B](ta: MyTypeClass[A], tb: MyTypeClass[B]): MyTypeClass[A & B] = {
      val cast: Any => Option[A & B] = any => Option.when(ta.cast(any).nonEmpty && tb.cast(any).nonEmpty)(any.asInstanceOf[A & B])
      MyTypeClass(
        (ta.options ::: tb.options).flatMap(cast),
        (a, b) => ta.eq(a, b) && tb.eq(a, b),
        cast,
      )
    }

  }

  implicit val aInstance: MyTypeClass[A] = MyTypeClass.make[A](A(-1), A(0), A(1), AB(-1, -1))
  implicit val bInstance: MyTypeClass[B] = MyTypeClass.make[B](B(-1), B(0), B(1), AB(1, 1))
  implicit val cInstance: MyTypeClass[C] = MyTypeClass.make[C](C(-1), C(0), C(1), ABC(2, 2, 2))

  // =====|  |=====

  private def makeTest[A](name: String)(expOptions: A*)(implicit ev: MyTypeClass[A]): TestSpec =
    test(name) {
      assert((ev.options, expOptions))(AssertionHelpers.testSeqElements(ev.eq(_, _)))
    }

  override def testSpec: TestSpec =
    suite("UnionAndIntersectionSpec")(
      suite("Basic")(
        makeTest[A]("A")(A(-1), A(0), A(1), AB(-1, -1)),
        makeTest[B]("B")(B(-1), B(0), B(1), AB(1, 1)),
        makeTest[C]("C")(C(-1), C(0), C(1), ABC(2, 2, 2)),
      ),
      suite("Union")(
        makeTest[A | B]("A | B")(A(-1), A(0), A(1), AB(-1, -1), B(-1), B(0), B(1), AB(1, 1)),
        makeTest[A | B | C]("A | B | C")(A(-1), A(0), A(1), AB(-1, -1), B(-1), B(0), B(1), AB(1, 1), C(-1), C(0), C(1), ABC(2, 2, 2)),
      ),
      suite("Intersection")(
        makeTest[A & B]("A | B")(AB(-1, -1), AB(1, 1)),
        makeTest[A & B & C]("A | B | C")(ABC(2, 2, 2)),
      ),
      suite("Complex")(
        makeTest[(A & B) | C]("(A & B) | C")(AB(-1, -1), AB(1, 1), C(-1), C(0), C(1), ABC(2, 2, 2)),
      ),
    )

}
