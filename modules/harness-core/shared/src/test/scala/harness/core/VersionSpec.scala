package harness.core

import cats.data.NonEmptyList
import cats.syntax.option.*
import cats.syntax.traverse.*
import harness.test.PlainHarnessSpec
import zio.test.*
import zio.test.Assertion.*

object VersionSpec extends PlainHarnessSpec {

  private def makePassingParseTest(string: String)(hasVPrefix: Boolean, suffix: Option[String], num0: Int, numN: Int*): TestSpec =
    test(string) {
      val version = Version.parse(string)
      assert(version)(isSome(equalTo(Version(hasVPrefix, NonEmptyList(num0, numN.toList), suffix)))) &&
      assert(version.map(_.toString))(isSome(equalTo(string)))
    }

  private def makeFailingParseTest(string: String): TestSpec =
    test(string) {
      assert(Version.parse(string))(isNone)
    }

  private def makeOrderingTest(pairs: String*): TestSpec = {
    val sortedVersions: List[Version] = pairs.toList.traverse(Version.parse).get
    suite(sortedVersions.mkString("sorts to: [", ", ", "]"))(
      sortedVersions.permutations.toList.map { versions =>
        test(versions.mkString("[", ", ", "]")) {
          assert(versions.sorted)(equalTo(sortedVersions))
        }
      } *,
    )
  }

  override def spec: TestSpec =
    suite("Version")(
      suite("parse")(
        suite("passes")(
          suite("basic")(
            makePassingParseTest("1")(false, None, 1),
            makePassingParseTest("1.2")(false, None, 1, 2),
            makePassingParseTest("1.2.3")(false, None, 1, 2, 3),
            makePassingParseTest("1.2.3.4")(false, None, 1, 2, 3, 4),
          ),
          suite("with v prefix")(
            makePassingParseTest("v1")(true, None, 1),
            makePassingParseTest("v1.2")(true, None, 1, 2),
            makePassingParseTest("v1.2.3")(true, None, 1, 2, 3),
            makePassingParseTest("v1.2.3.4")(true, None, 1, 2, 3, 4),
          ),
          suite("with suffix")(
            makePassingParseTest("1-SNAPSHOT")(false, "-SNAPSHOT".some, 1),
            makePassingParseTest("1.2-SNAPSHOT")(false, "-SNAPSHOT".some, 1, 2),
            makePassingParseTest("1.2.3-SNAPSHOT")(false, "-SNAPSHOT".some, 1, 2, 3),
            makePassingParseTest("1.2.3.4-SNAPSHOT")(false, "-SNAPSHOT".some, 1, 2, 3, 4),
          ),
          suite("with v prefix and suffix")(
            makePassingParseTest("v1-SNAPSHOT")(true, "-SNAPSHOT".some, 1),
            makePassingParseTest("v1.2-SNAPSHOT")(true, "-SNAPSHOT".some, 1, 2),
            makePassingParseTest("v1.2.3-SNAPSHOT")(true, "-SNAPSHOT".some, 1, 2, 3),
            makePassingParseTest("v1.2.3.4-SNAPSHOT")(true, "-SNAPSHOT".some, 1, 2, 3, 4),
          ),
          makePassingParseTest("v1.2.3.4---SNAPSHOT")(true, "---SNAPSHOT".some, 1, 2, 3, 4),
        ),
        suite("fails")(
          makeFailingParseTest(" 1.2.3"),
          makeFailingParseTest("1.2.3 "),
          makeFailingParseTest("v 1.2.3"),
          makeFailingParseTest("1.2.3 SNAPSHOT"),
        ),
      ),
      suite("ordering")(
        makeOrderingTest(),
        makeOrderingTest("1"),
        makeOrderingTest("1", "1.1"),
        makeOrderingTest("1.1", "1.2"),
        makeOrderingTest("1", "1.1-RC1", "1.1"),
        makeOrderingTest("1", "1.1-RC1", "1.1", "1.1.1", "1.1.2", "1.2", "1.2"),
      ),
      suite("equals")(
        test("basic") { assertTrue(Version.parseUnsafe("0") == Version.parseUnsafe("0")) },
        test("extra zeros") { assertTrue(Version.parseUnsafe("3") == Version.parseUnsafe("3.0.0")) },
        test("with v") { assertTrue(Version.parseUnsafe("v3") == Version.parseUnsafe("3.0.0")) },
      ),
    )

}
