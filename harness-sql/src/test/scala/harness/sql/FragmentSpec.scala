package harness.sql

import cats.data.NonEmptyList
import cats.syntax.option.*
import cats.syntax.traverse.*
import harness.sql.query.*
import harness.test.AssertionHelpers.*
import harness.test.DefaultHarnessSpec
import zio.test.*
import zio.test.Assertion.*

object FragmentSpec extends DefaultHarnessSpec {

  private def makeTest(name: String)(_frag: => Fragment)(expSql: String): TestSpec =
    test(name) {
      val frag = _frag
      assert(frag)(equalTo(expSql).imap[Fragment]("sql")(_.sql))
    }

  private val colRef1: ColRef = ColRef("tab_1", "col", None)
  private val colRef2: ColRef = ColRef("tab_2", "col", None)
  
  override def spec: TestSpec =
    suite("FragmentSpec")(
      makeTest("ColRef")(fr"$colRef1 IS NULL")("tab_1.col IS NULL"),
    )

}
