package harness.core

import harness.zio.test.*
import zio.test.*
import zio.test.Assertion.*

object StringOpsSpec extends DefaultHarnessSpec {

  private def makeTest(input: String, convert: String => String, exp: => String): TestSpec =
    test(s"'$input' -> '$exp'")(assert(convert(input))(equalTo(exp)))

  /*
  aBC > a_bc

  ABC > abc

  abcDefHgi > abc_def_hgi

  AbcDefHgi > abc_def_hgi

  aBCdef > a_b_cdef

  ABCdef > ab_cdef

  abc1Def > abc_1_def

  Abc1Def > abc_1_def

  AB1DE > ab_1_de

   */

  override def testSpec: TestSpec =
    suite("StringOps")(
      suite("camelToSnake")(
        makeTest("aBC", _.camelToSnake, "a_bc"),
        makeTest("ABC", _.camelToSnake, "abc"),
        makeTest("abcDefHgi", _.camelToSnake, "abc_def_hgi"),
        makeTest("AbcDefHgi", _.camelToSnake, "abc_def_hgi"),
        makeTest("aBCdef", _.camelToSnake, "a_b_cdef"),
        makeTest("ABCdef", _.camelToSnake, "ab_cdef"),
        makeTest("abc1Def", _.camelToSnake, "abc_1_def"),
        makeTest("Abc1Def", _.camelToSnake, "abc_1_def"),
        makeTest("AB1DE", _.camelToSnake, "ab_1_de"),
      ),
      suite("snakeToLowerCamel")(
        makeTest("a_b_c", _.snakeToLowerCamel, "aBC"),
        makeTest("abc_def_hgi", _.snakeToLowerCamel, "abcDefHgi"),
        makeTest("abc_1_def", _.snakeToLowerCamel, "abc1Def"),
      ),
      suite("snakeToUpperCamel")(
        makeTest("a_b_c", _.snakeToUpperCamel, "ABC"),
        makeTest("abc_def_hgi", _.snakeToUpperCamel, "AbcDefHgi"),
        makeTest("abc_1_def", _.snakeToUpperCamel, "Abc1Def"),
      ),
      suite("dashToSnake")(
        makeTest("a-b-c", _.dashToSnake, "a_b_c"),
      ),
      suite("snakeToDash")(
        makeTest("a_b_c", _.snakeToDash, "a-b-c"),
      ),
    )

}
