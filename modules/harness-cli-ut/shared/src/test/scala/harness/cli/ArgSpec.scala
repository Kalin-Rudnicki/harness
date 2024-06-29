package harness.cli

import harness.zio.test.*
import zio.test.*
import zio.test.Assertion.*

object ArgSpec extends DefaultHarnessSpec {

  private val parseSucceeds: TestSpec = {
    def makeTest(input: String)(expArgs: Arg*): TestSpec =
      test(input) {
        assert(Arg.parse(input))(isRight(equalTo(expArgs.toList)))
      }

    suite("parseSucceeds")(
      makeTest("[-]1")(Arg.Value("[-]1")),
      makeTest("[-]-1")(Arg.Value("-1")),
      makeTest("[-]--1")(Arg.Value("--1")),
      makeTest("--")(Arg.Value("--")),
      makeTest("1")(Arg.Value("1")),
      makeTest("-1")(Arg.ShortParamSingle(ShortName.unsafe('1'))),
      makeTest("-12")(Arg.ShortParamMulti(ShortName.unsafe('1'), 0), Arg.ShortParamMulti(ShortName.unsafe('2'), 1)),
      makeTest("-1=value")(Arg.ShortParamSingleWithValue(ShortName.unsafe('1'), "value")),
      makeTest("--1")(Arg.LongParam(LongName.unsafe("1"))),
      makeTest("--1=value")(Arg.LongParamWithValue(LongName.unsafe("1"), "value")),
    )
  }

  private val parseFails: TestSpec = {
    def makeTest(input: String): TestSpec =
      test(input) {
        assert(Arg.parse(input))(isLeft)
      }

    suite("parseFails")(
      makeTest("-ab=value"),
    )
  }

  private val identity: TestSpec = {
    def makeTest(inputs: String*): TestSpec = {
      val inputList = inputs.toList
      test(inputList.mkString("{", ", ", "}")) {
        assert(IndexedArgs.parse(inputList).map(IndexedArgs.format))(isRight(equalTo(inputList)))
      }
    }

    suite("identity")(
      makeTest(),
      makeTest("1"),
      makeTest("[-]1"),
      makeTest("[-]-1"),
      makeTest("[-]--1"),
      makeTest("--"),
      makeTest("-1"),
      makeTest("-123"),
      makeTest("-1=value"),
      makeTest("--1"),
      makeTest("--123-456"),
      makeTest("--1=value"),
      makeTest("-123", "456", "-789"),
      makeTest("123", "-456", "789"),
    )
  }

  override def testSpec: TestSpec =
    suite("ArgSpec")(
      parseSucceeds,
      parseFails,
      identity,
    )

}
