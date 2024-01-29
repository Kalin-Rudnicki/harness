package harness.cli

import cats.data.{Ior, NonEmptyList}
import cats.syntax.option.*
import harness.test.AssertionHelpers.*
import harness.test.PlainHarnessSpec
import scala.annotation.unused
import zio.test.*
import zio.test.Assertion.*

object ParserSpec extends PlainHarnessSpec {

  private def parserSuite[T](name: String)(parser: Parser[T])(
      tests: (FinalizedParser[T] ?=> TestSpec)*,
  ): TestSpec =
    suite(name) {
      val finalizedParser: FinalizedParser[T] = parser.finalized
      tests.map(_(using finalizedParser))
    }

  private def makeTest[T](inputs: String*)(assertion: Assertion[FinalizedParser.Result[T]])(implicit parser: FinalizedParser[T]): TestSpec = {
    val inputList = inputs.toList
    test(inputs.mkString("{", ", ", s"}")) {
      val res = parser(inputList)
      if (!assertion.test(res)) println(res)
      assert(res)(assertion)
    }
  }

  private def isSuccess[T](valueAssertion: Assertion[T]): Assertion[FinalizedParser.Result[T]] =
    valueAssertion.imap[FinalizedParser.Result[T]]("FinalizedParser.Result") { case FinalizedParser.Result.Success(value) => value }

  private def isParseFail(failAssertion: Assertion[ParsingFailure]): Assertion[FinalizedParser.Result[Any]] =
    isSubtype[FinalizedParser.Result.ParseFail](failAssertion.imap[FinalizedParser.Result.ParseFail]("fail")(_.fail))

  @unused
  private def isBuildFail(duplicateParamAssertion: Assertion[Name]): Assertion[FinalizedParser.Result[Any]] =
    isSubtype[FinalizedParser.Result.BuildFail](duplicateParamAssertion.imap[FinalizedParser.Result.BuildFail]("duplicateParam")(_.duplicateParam))

  private def isHelp(helpExtraAssertion: Assertion[Boolean], messageAssertion: Assertion[HelpMessage]): Assertion[FinalizedParser.Result[Any]] =
    isSubtype[FinalizedParser.Result.Help](
      helpExtraAssertion.imap[FinalizedParser.Result.Help]("helpExtra")(_.helpExtra) &&
        messageAssertion.imap[FinalizedParser.Result.Help]("message")(_.message),
    )

  private val help: TestSpec =
    parserSuite("help")(
      Parser.unit,
    )(
      makeTest("-H")(isHelp(equalTo(true), anything)),
      makeTest("--help-extra")(isHelp(equalTo(true), anything)),
      makeTest("-h")(isHelp(equalTo(false), anything)),
      makeTest("--help")(isHelp(equalTo(false), anything)),
      makeTest("--help-extra", "--help", "-h")(isHelp(equalTo(true), anything)),
      makeTest("--help", "-h", "--help-extra")(isHelp(equalTo(true), anything)),
      makeTest("-H", "--help", "-h")(isHelp(equalTo(true), anything)),
      makeTest("--help", "-h", "-H")(isHelp(equalTo(true), anything)),
    )

  private val simplePresent: TestSpec =
    parserSuite("simplePresent") {
      Parser.present(LongName.unsafe("name"), ())
    }(
      makeTest("--name")(isSuccess(equalTo(()))),
      makeTest("-n")(isSuccess(equalTo(()))),
      makeTest()(isParseFail(anything)),
    )

  private val simpleFlag: TestSpec =
    parserSuite("simpleFlag") {
      Parser.flag(LongName.unsafe("name"))
    }(
      makeTest("--name")(isSuccess(equalTo(true))),
      makeTest("-n")(isSuccess(equalTo(true))),
      makeTest()(isSuccess(equalTo(false))),
    )

  private val simpleToggle: TestSpec =
    parserSuite("simpleToggle") {
      Parser.toggle.prefixFalse(LongName.unsafe("no"), LongName.unsafe("name"))
    }(
      makeTest("--name")(isSuccess(equalTo(true))),
      makeTest("-N")(isSuccess(equalTo(true))),
      makeTest("--no-name")(isSuccess(equalTo(false))),
      makeTest("-n")(isSuccess(equalTo(false))),
      makeTest()(isParseFail(anything)),
    )

  private val simpleValue: TestSpec =
    parserSuite("simpleValue") {
      Parser.value[Int](LongName.unsafe("int"))
    }(
      makeTest("--int", "1")(isSuccess(equalTo(1))),
      makeTest("--int=1")(isSuccess(equalTo(1))),
      makeTest("-i", "1")(isSuccess(equalTo(1))),
      makeTest("-i=1")(isSuccess(equalTo(1))),
      makeTest()(isParseFail(anything)),
    )

  private val simpleValueOptional: TestSpec =
    parserSuite("simpleValueOptional") {
      Parser.value[Int](LongName.unsafe("int")).optional
    }(
      makeTest("--int", "1")(isSuccess(isSome(equalTo(1)))),
      makeTest("--int=1")(isSuccess(isSome(equalTo(1)))),
      makeTest("-i", "1")(isSuccess(isSome(equalTo(1)))),
      makeTest("-i=1")(isSuccess(isSome(equalTo(1)))),
      makeTest()(isSuccess(isNone)),
    )

  private val simpleValueDefault: TestSpec =
    parserSuite("simpleValueDefault") {
      Parser.value[Int](LongName.unsafe("int")).default(0)
    }(
      makeTest("--int", "1")(isSuccess(equalTo(1))),
      makeTest("--int=1")(isSuccess(equalTo(1))),
      makeTest("-i", "1")(isSuccess(equalTo(1))),
      makeTest("-i=1")(isSuccess(equalTo(1))),
      makeTest()(isSuccess(equalTo(0))),
    )

  private val simpleValueList: TestSpec =
    parserSuite("simpleValueList") {
      Parser.values.list[Int](LongName.unsafe("int"))
    }(
      makeTest("--int", "1")(isSuccess(equalTo(List(1)))),
      makeTest("--int=1")(isSuccess(equalTo(List(1)))),
      makeTest("-i", "1")(isSuccess(equalTo(List(1)))),
      makeTest("-i=1")(isSuccess(equalTo(List(1)))),
      makeTest()(isSuccess(equalTo(List()))),
      makeTest("--int", "1", "--int=2", "-i", "3", "-i=4", "--int", "5", "--int=6", "-i", "7", "-i=8")(isSuccess(equalTo(List(1, 2, 3, 4, 5, 6, 7, 8)))),
    )

  private val simpleValueNel: TestSpec =
    parserSuite("simpleValueNel") {
      Parser.values.nel[Int](LongName.unsafe("int"))
    }(
      makeTest("--int", "1")(isSuccess(equalTo(NonEmptyList.of(1)))),
      makeTest("--int=1")(isSuccess(equalTo(NonEmptyList.of(1)))),
      makeTest("-i", "1")(isSuccess(equalTo(NonEmptyList.of(1)))),
      makeTest("-i=1")(isSuccess(equalTo(NonEmptyList.of(1)))),
      makeTest()(isParseFail(anything)),
      makeTest("--int", "1", "--int=2", "-i", "3", "-i=4", "--int", "5", "--int=6", "-i", "7", "-i=8")(isSuccess(equalTo(NonEmptyList.of(1, 2, 3, 4, 5, 6, 7, 8)))),
    )

  private final case class Person(
      firstName: String,
      lastName: String,
      age: Option[Int],
  )

  private val personParser: Parser[Person] =
    (
      Parser.value[String](LongName.unsafe("first-name"), helpHint = "Persons first name" :: Nil) &&
        Parser.value[String](LongName.unsafe("last-name"), helpHint = "Persons last name" :: Nil) &&
        Parser.value[Int](LongName.unsafe("age"), helpHint = "Persons age" :: "(Int)" :: Nil).optional
    ).map(Person.apply)

  private val basePerson: TestSpec =
    parserSuite("basePerson")(
      personParser,
    )(
      makeTest("--first-name=FIRST", "--last-name=LAST")(isSuccess(equalTo(Person("FIRST", "LAST", None)))),
      makeTest("--first-name=FIRST", "--last-name=LAST", "--age=100")(isSuccess(equalTo(Person("FIRST", "LAST", 100.some)))),
      makeTest("-f=FIRST", "-l=LAST")(isSuccess(equalTo(Person("FIRST", "LAST", None)))),
      makeTest("-f=FIRST", "-l=LAST", "-a=100")(isSuccess(equalTo(Person("FIRST", "LAST", 100.some)))),
      makeTest("-f=FIRST")(isParseFail(anything)),
      makeTest("-l=LAST")(isParseFail(anything)),
      makeTest()(isParseFail(anything)),
    )

  private val optionalPerson: TestSpec =
    parserSuite("optionalPerson")(
      personParser.optional,
    )(
      makeTest("--first-name=FIRST", "--last-name=LAST")(isSuccess(isSome(equalTo(Person("FIRST", "LAST", None))))),
      makeTest("--first-name=FIRST", "--last-name=LAST", "--age=100")(isSuccess(isSome(equalTo(Person("FIRST", "LAST", 100.some))))),
      makeTest("-f=FIRST", "-l=LAST")(isSuccess(isSome(equalTo(Person("FIRST", "LAST", None))))),
      makeTest("-f=FIRST", "-l=LAST", "-a=100")(isSuccess(isSome(equalTo(Person("FIRST", "LAST", 100.some))))),
      makeTest("-f=FIRST")(isParseFail(anything)),
      makeTest("-l=LAST")(isParseFail(anything)),
      makeTest()(isSuccess(isNone)),
    )

  private val defaultPerson: TestSpec =
    parserSuite("defaultPerson")(
      personParser.default(Person("F", "L", 0.some)),
    )(
      makeTest("--first-name=FIRST", "--last-name=LAST")(isSuccess(equalTo(Person("FIRST", "LAST", None)))),
      makeTest("--first-name=FIRST", "--last-name=LAST", "--age=100")(isSuccess(equalTo(Person("FIRST", "LAST", 100.some)))),
      makeTest("-f=FIRST", "-l=LAST")(isSuccess(equalTo(Person("FIRST", "LAST", None)))),
      makeTest("-f=FIRST", "-l=LAST", "-a=100")(isSuccess(equalTo(Person("FIRST", "LAST", 100.some)))),
      makeTest("-f=FIRST")(isParseFail(anything)),
      makeTest("-l=LAST")(isParseFail(anything)),
      makeTest()(isSuccess(equalTo(Person("F", "L", 0.some)))),
    )

  private val iorParser: Parser[Ior[String, Int]] =
    Parser.firstOf(
      (
        Parser.value[String](LongName.unsafe("left")) &&
          Parser.value[Int](LongName.unsafe("right"))
      ).map(Ior.Both.apply),
      Parser.value[String](LongName.unsafe("left")).map(Ior.Left.apply),
      Parser.value[Int](LongName.unsafe("right")).map(Ior.Right.apply),
    )

  private val baseIor: TestSpec =
    parserSuite("baseIor")(
      iorParser,
    )(
      makeTest("--left=LEFT", "--right=1")(isSuccess(equalTo(Ior.Both("LEFT", 1)))),
      makeTest("-l=LEFT", "-r=1")(isSuccess(equalTo(Ior.Both("LEFT", 1)))),
      makeTest("--left=LEFT")(isSuccess(equalTo(Ior.Left("LEFT")))),
      makeTest("-l=LEFT")(isSuccess(equalTo(Ior.Left("LEFT")))),
      makeTest("--right=1")(isSuccess(equalTo(Ior.Right(1)))),
      makeTest("-r=1")(isSuccess(equalTo(Ior.Right(1)))),
      makeTest()(isParseFail(anything)),
    )

  private val optionalIor: TestSpec =
    parserSuite("optionalIor")(
      iorParser.optional,
    )(
      makeTest("--left=LEFT", "--right=1")(isSuccess(isSome(equalTo(Ior.Both("LEFT", 1))))),
      makeTest("-l=LEFT", "-r=1")(isSuccess(isSome(equalTo(Ior.Both("LEFT", 1))))),
      makeTest("--left=LEFT")(isSuccess(isSome(equalTo(Ior.Left("LEFT"))))),
      makeTest("-l=LEFT")(isSuccess(isSome(equalTo(Ior.Left("LEFT"))))),
      makeTest("--right=1")(isSuccess(isSome(equalTo(Ior.Right(1))))),
      makeTest("-r=1")(isSuccess(isSome(equalTo(Ior.Right(1))))),
      makeTest()(isSuccess(isNone)),
    )

  private val defaultIor: TestSpec =
    parserSuite("defaultIor")(
      iorParser.default(Ior.Right(0)),
    )(
      makeTest("--left=LEFT", "--right=1")(isSuccess(equalTo(Ior.Both("LEFT", 1)))),
      makeTest("-l=LEFT", "-r=1")(isSuccess(equalTo(Ior.Both("LEFT", 1)))),
      makeTest("--left=LEFT")(isSuccess(equalTo(Ior.Left("LEFT")))),
      makeTest("-l=LEFT")(isSuccess(equalTo(Ior.Left("LEFT")))),
      makeTest("--right=1")(isSuccess(equalTo(Ior.Right(1)))),
      makeTest("-r=1")(isSuccess(equalTo(Ior.Right(1)))),
      makeTest()(isSuccess(equalTo(Ior.Right(0)))),
    )

  private sealed trait SumType
  private object SumType {
    final case class Case1(food: String, person: Person) extends SumType
    final case class Case2(value: Option[Int]) extends SumType
  }

  private val sumTypeParser: Parser[SumType] =
    Parser.firstOf(
      (
        Parser.present(LongName.unsafe("case-1"), ()) ##>
          Parser.value[String](LongName.unsafe("food")) &&
          personParser.default(Person("F", "L", None))
      ).map(SumType.Case1.apply),
      (
        Parser.present(LongName.unsafe("case-2"), ()) ##>
          Parser.value[Int](LongName.unsafe("value")).optional
      ).map(SumType.Case2.apply),
    )

  private val baseSumType: TestSpec =
    parserSuite("baseSumType")(
      sumTypeParser,
    )(
      makeTest("--case-1", "--food=PASTA")(isSuccess(equalTo(SumType.Case1("PASTA", Person("F", "L", None))))),
      makeTest("--case-1", "--food=PASTA", "--first-name=FIRST", "--last-name=LAST")(isSuccess(equalTo(SumType.Case1("PASTA", Person("FIRST", "LAST", None))))),
      makeTest("--case-1", "--food=PASTA", "--first-name=FIRST", "--last-name=LAST", "--age=100")(isSuccess(equalTo(SumType.Case1("PASTA", Person("FIRST", "LAST", 100.some))))),
      makeTest("--case-2")(isSuccess(equalTo(SumType.Case2(None)))),
      makeTest("--case-2", "--value=1")(isSuccess(equalTo(SumType.Case2(1.some)))),
      makeTest("--case-1")(isParseFail(anything)),
      makeTest()(isParseFail(anything)),
    )

  private val optionalSumType: TestSpec =
    parserSuite("optionalSumType")(
      sumTypeParser.optional,
    )(
      makeTest("--case-1", "--food=PASTA")(isSuccess(isSome(equalTo(SumType.Case1("PASTA", Person("F", "L", None)))))),
      makeTest("--case-1", "--food=PASTA", "--first-name=FIRST", "--last-name=LAST")(isSuccess(isSome(equalTo(SumType.Case1("PASTA", Person("FIRST", "LAST", None)))))),
      makeTest("--case-1", "--food=PASTA", "--first-name=FIRST", "--last-name=LAST", "--age=100")(isSuccess(isSome(equalTo(SumType.Case1("PASTA", Person("FIRST", "LAST", 100.some)))))),
      makeTest("--case-2")(isSuccess(isSome(equalTo(SumType.Case2(None))))),
      makeTest("--case-2", "--value=1")(isSuccess(isSome(equalTo(SumType.Case2(1.some))))),
      makeTest("--case-1")(isParseFail(anything)),
      makeTest()(isSuccess(isNone)),
    )

  private val defaultSumType: TestSpec =
    parserSuite("defaultSumType")(
      sumTypeParser.default(SumType.Case2(None)),
    )(
      makeTest("--case-1", "--food=PASTA")(isSuccess(equalTo(SumType.Case1("PASTA", Person("F", "L", None))))),
      makeTest("--case-1", "--food=PASTA", "--first-name=FIRST", "--last-name=LAST")(isSuccess(equalTo(SumType.Case1("PASTA", Person("FIRST", "LAST", None))))),
      makeTest("--case-1", "--food=PASTA", "--first-name=FIRST", "--last-name=LAST", "--age=100")(isSuccess(equalTo(SumType.Case1("PASTA", Person("FIRST", "LAST", 100.some))))),
      makeTest("--case-2")(isSuccess(equalTo(SumType.Case2(None)))),
      makeTest("--case-2", "--value=1")(isSuccess(equalTo(SumType.Case2(1.some)))),
      makeTest("--case-1")(isParseFail(anything)),
      makeTest()(isSuccess(equalTo(SumType.Case2(None)))),
    )

  override def spec: TestSpec =
    suite("ParserSpec")(
      help,
      suite("simple")(
        simplePresent,
        simpleFlag,
        simpleToggle,
        simpleValue,
        simpleValueOptional,
        simpleValueDefault,
        simpleValueList,
        simpleValueNel,
      ),
      suite("person")(
        basePerson,
        optionalPerson,
        defaultPerson,
      ),
      suite("ior")(
        baseIor,
        optionalIor,
        defaultIor,
      ),
      suite("sumType")(
        baseSumType,
        optionalSumType,
        defaultSumType,
      ),
    )

}
