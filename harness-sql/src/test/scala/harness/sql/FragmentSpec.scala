package harness.sql

import harness.sql.query.*
import harness.test.AssertionHelpers.*
import harness.test.PlainHarnessSpec
import scala.annotation.unused
import zio.test.*
import zio.test.Assertion.*

object FragmentSpec extends PlainHarnessSpec {

  private def makeTest(name: String)(_frag: => Fragment)(expSql: String): TestSpec =
    test(name) {
      val frag = _frag
      assert(frag)(equalTo(expSql).imap[Fragment]("sql")(_.sql))
    }

  private val colRef1: ColRef = ColRef("tab_1", "col", None)
  @unused
  private val colRef2: ColRef = ColRef("tab_2", "col", None)

  override def spec: TestSpec =
    suite("FragmentSpec")(
      makeTest("ColRef")(fr"$colRef1 IS NULL")("tab_1.col IS NULL"),
      makeTest("CreateTable")(fr"CREATE TABLE ${"schema"}.${"table"} (${"id UUID PRIMARY KEY"})")("CREATE TABLE schema.table (id UUID PRIMARY KEY)"),
    )

}
