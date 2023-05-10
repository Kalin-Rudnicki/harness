package harness.csv

import harness.test.*
import zio.test.*
import zio.test.Assertion.*

object ParserSpec extends DefaultHarnessSpec {

  private def passingTest(name: String)(csv: String, exp: List[List[String]]): TestSpec =
    test(name) {
      assert(Parser.parse(csv))(isRight(equalTo(exp)))
    }

  override def spec: TestSpec =
    suite("ParserSpec")(
      suite("passes")(
        passingTest("case-1")(
          "",
          List("") :: Nil,
        ),
        passingTest("case-2")(
          ",,",
          List("", "", "") :: Nil,
        ),
        passingTest("case-3")(
          ",,\n,,",
          List("", "", "") :: List("", "", "") :: Nil,
        ),
        passingTest("case-4")(
          "A,B,C",
          List("A", "B", "C") :: Nil,
        ),
        passingTest("case-5")(
          "A,B,\"C\nC\"",
          List("A", "B", "C\nC") :: Nil,
        ),
        passingTest("case-6")(
          "A,B,\"C\nC\",D",
          List("A", "B", "C\nC", "D") :: Nil,
        ),
        passingTest("case-7")(
          "A,B,\"C\nC\",\"D\"\"\"",
          List("A", "B", "C\nC", "D\"") :: Nil,
        ),
        passingTest("case-8")(
          "ABC,DEF\nGHI,JKL",
          List("ABC", "DEF") :: List("GHI", "JKL") :: Nil,
        ),
      ),
    )

}
