package harness.zio.test

import cats.data.NonEmptyList
import zio.test.*
import zio.test.Assertion.*

object AssertionHelpers {

  extension [A](self: Assertion[A]) {

    def imap[B](name: String)(f: PartialFunction[B, A]): Assertion[B] =
      assertionRec[B, A](name)(self)(f.lift(_))

  }

  extension [A](self: Assertion[Seq[A]]) {

    def toNelAssertion: Assertion[NonEmptyList[A]] =
      self.imap("NonEmptyList")(_.toList)

  }

  def assertSeq[A](assertions: Assertion[A]*): Assertion[Seq[A]] =
    assertions.toList.zipWithIndex.foldLeft[Assertion[Seq[A]]](
      hasSize(equalTo(assertions.size)),
    ) { case (accum, (a, i)) =>
      accum && hasAt(i)(a)
    }

}
