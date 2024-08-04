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

  def seqsHaveSameSize[A]: Assertion[(Seq[A], Seq[A])] =
    Assertion.assertion("Seqs have same size") { case (seqA, seqB) => seqA.size == seqB.size }

  def testSeqElements[A](test: (A, A) => Boolean)(makeMessage: (A, A, Int) => ErrorMessage): Assertion[(Seq[A], Seq[A])] =
    Assertion(
      TestArrow.make[(Seq[A], Seq[A]), Boolean] { case (seqA, seqB) =>
        val sizes =
          TestTrace.boolean(seqA.size == seqB.size) {
            ErrorMessage.text("Size of") ++
              ErrorMessage.pretty(seqA) ++
              ErrorMessage.equals ++
              ErrorMessage.pretty(seqB)
          }

        seqA
          .zip(seqB)
          .zipWithIndex
          .map { case ((a, b), i) =>
            TestTrace.boolean(test(a, b))(makeMessage(a, b, i))
          }
          .foldLeft(sizes) { (acc, t) => acc && t }
      },
    )
  def testSeqElements[A](test: (A, A) => Boolean): Assertion[(Seq[A], Seq[A])] =
    testSeqElements(test) { (a, b, i) =>
      ErrorMessage.text("Seq test") +
        ErrorMessage.choice("passed", "failed") +
        ErrorMessage.text(s"at index $i") +
        ErrorMessage.text("with elems:") ++
        ErrorMessage.text("left =") +
        ErrorMessage.pretty(a) ++
        ErrorMessage.text("right =") +
        ErrorMessage.pretty(b)
    }

}
