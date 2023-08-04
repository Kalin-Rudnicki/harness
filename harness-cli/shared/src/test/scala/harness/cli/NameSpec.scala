package harness.cli

import cats.syntax.option.*
import harness.test.AssertionHelpers.*
import harness.test.PlainHarnessSpec
import scala.reflect.ClassTag
import zio.*
import zio.test.*
import zio.test.Assertion.*

object NameSpec extends PlainHarnessSpec {

  private val longName: TestSpec = {
    val applySucceeds: TestSpec = {
      def makeTest(input: String)(expFirstChar: Char): TestSpec =
        test(input) {
          assert(LongName(input)) {
            isRight {
              equalTo(expFirstChar).imap[LongName]("firstChar")(_.firstChar.name) &&
              equalTo(input).imap[LongName]("name")(_.name)
            }
          }
        }

      suite("applySucceeds")(
        makeTest("a")('a'),
        makeTest("ab")('a'),
        makeTest("a-b")('a'),
        makeTest("1")('1'),
        makeTest("a-1")('a'),
        makeTest("a-bc-def")('a'),
        makeTest("AbCd")('A'),
      )
    }

    val applyFails: TestSpec = {
      def makeTest(input: String): TestSpec =
        test(input) {
          assert(LongName(input))(isLeft)
        }

      suite("applyFails")(
        makeTest("?"),
        makeTest("-"),
        makeTest("a--b"),
      )
    }

    val dashSucceeds: TestSpec = {
      def makeTest(input1: String, input2: String)(expFirstChar: Char): TestSpec =
        test(s"$input1 - $input2") {
          assert {
            for {
              res1 <- LongName(input1)
              res2 <- LongName(input2)
            } yield res1 - res2
          } {
            isRight {
              equalTo(expFirstChar).imap[LongName]("firstChar")(_.firstChar.name) &&
              equalTo(s"$input1-$input2").imap[LongName]("name")(_.name)
            }
          }
        }

      suite("dashSucceeds")(
        makeTest("a", "b")('a'),
        makeTest("ab", "cd")('a'),
        makeTest("ab-cd", "ef-gh")('a'),
      )
    }

    suite("longName")(
      applySucceeds,
      applyFails,
      dashSucceeds,
    )
  }

  private val shortName: TestSpec = {
    val applySucceeds: TestSpec = {
      def makeTest[T <: ShortName, ET: ClassTag](input: Char, builder: ShortNameBuilder[T]): TestSpec =
        test(input.toString) {
          assert(builder(input)) {
            isRight {
              isSubtype[ET](anything) &&
              equalTo(input).imap("name")(_.name)
            }
          }
        }

      suite("applySucceeds")(
        makeTest[ShortName.LowerLetter, ShortName.LowerLetter]('a', ShortName.LowerLetter),
        makeTest[ShortName.UpperLetter, ShortName.UpperLetter]('A', ShortName.UpperLetter),
        makeTest[ShortName.Digit, ShortName.Digit]('1', ShortName.Digit),
        makeTest[ShortName.Letter, ShortName.LowerLetter]('a', ShortName.Letter),
        makeTest[ShortName.Letter, ShortName.UpperLetter]('A', ShortName.Letter),
        makeTest[ShortName, ShortName.LowerLetter]('a', ShortName),
        makeTest[ShortName, ShortName.UpperLetter]('A', ShortName),
        makeTest[ShortName, ShortName.Digit]('1', ShortName),
      )
    }
    val applyFails: TestSpec = {
      def makeTest[T <: ShortName](input: Char, builder: ShortNameBuilder[T]): TestSpec =
        test(input.toString) {
          assert(builder(input))(isLeft)
        }

      suite("applyFails")(
        makeTest('A', ShortName.LowerLetter),
        makeTest('1', ShortName.LowerLetter),
        makeTest('?', ShortName.LowerLetter),
        makeTest('a', ShortName.UpperLetter),
        makeTest('1', ShortName.UpperLetter),
        makeTest('?', ShortName.UpperLetter),
        makeTest('1', ShortName.Letter),
        makeTest('?', ShortName.Letter),
        makeTest('a', ShortName.Digit),
        makeTest('A', ShortName.Digit),
        makeTest('?', ShortName.Digit),
        makeTest('?', ShortName),
      )
    }

    suite("shortName")(
      applySucceeds,
      applyFails,
    )
  }

  override def spec: TestSpec =
    suite("NameSpec")(
      longName,
      shortName,
    )

}
