package harness.csv

import cats.syntax.option.*
import harness.test.*
import zio.test.*
import zio.test.Assertion.*

object ParserSpec extends PlainHarnessSpec {

  private def passingTest(name: String)(csv: String, exp: List[List[Option[String]]]): TestSpec =
    test(name) {
      assert(Parser.parse(csv).map(_.map(_.toList)))(isRight(equalTo(exp)))
    }

  override def spec: TestSpec =
    suite("ParserSpec")(
      suite("passes")(
        passingTest("case-1")(
          "",
          Nil,
        ),
        passingTest("case-2")(
          ",,",
          List(None, None, None) :: Nil,
        ),
        passingTest("case-3")(
          "\"\",,\n,,",
          List("".some, None, None) :: List(None, None, None) :: Nil,
        ),
        passingTest("case-4")(
          "A,B,C",
          List("A".some, "B".some, "C".some) :: Nil,
        ),
        passingTest("case-5")(
          "A,B,\"C\nC\"",
          List("A".some, "B".some, "C\nC".some) :: Nil,
        ),
        passingTest("case-6")(
          "A,B,\"C\nC\",D",
          List("A".some, "B".some, "C\nC".some, "D".some) :: Nil,
        ),
        passingTest("case-7")(
          "A,B,\"C\nC\",\"D\"\"\"",
          List("A".some, "B".some, "C\nC".some, "D\"".some) :: Nil,
        ),
        passingTest("case-8")(
          "ABC,DEF\nGHI,JKL",
          List("ABC".some, "DEF".some) :: List("GHI".some, "JKL".some) :: Nil,
        ),
        passingTest("case-9")(
          "ABC,DEF\nGHI,JKL\n",
          List("ABC".some, "DEF".some) :: List("GHI".some, "JKL".some) :: Nil,
        ),
      ),
    )

}
