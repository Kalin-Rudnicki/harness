package harness.core

import cats.data.NonEmptyList
import cats.syntax.option.*
import harness.zio.test.*
import zio.internal.stacktracer.SourceLocation
import zio.test.*
import zio.test.Assertion.*

object VersionSpec extends DefaultHarnessSpec {

  private object versionSpec {

    private def passingParseTest(string: String)(hasVPrefix: Boolean, suffix: Option[Version.Suffix.Repr], num0: Int, numN: Int*)(implicit loc: SourceLocation): TestSpec =
      test(string) {
        val version = Version.parse(string)
        assert(version)(isSome(equalTo(Version(hasVPrefix, NonEmptyList(num0, numN.toList), suffix.map(_.toSuffix))))) &&
        assert(version.map(_.toString))(isSome(equalTo(string)))
      }

    private def failingParseTest(string: String)(implicit loc: SourceLocation): TestSpec =
      test(string) {
        assert(Version.parse(string))(isNone)
      }

    private def orderingTest(smaller: String, greater: String)(implicit loc: SourceLocation): TestSpec =
      test(s"$smaller < $greater") {
        val smallerVersion = Version.parseUnsafe(smaller)
        val greaterVersion = Version.parseUnsafe(greater)
        assertTrue(
          Version.ordering.lt(smallerVersion, greaterVersion),
          Version.ordering.gt(greaterVersion, smallerVersion),
        )
      }

    private def equalsTest(a: String, b: String)(implicit loc: SourceLocation): TestSpec =
      test(s"$a == $b") {
        assertTrue(Version.parseUnsafe(a) == Version.parseUnsafe(b))
      }

    val spec: TestSpec =
      suite("version")(
        suite("parse")(
          suite("passes")(
            suite("basic")(
              passingParseTest("1")(false, None, 1),
              passingParseTest("1.2")(false, None, 1, 2),
              passingParseTest("1.2.3")(false, None, 1, 2, 3),
              passingParseTest("1.2.3.4")(false, None, 1, 2, 3, 4),
            ),
            suite("with v prefix")(
              passingParseTest("v1")(true, None, 1),
              passingParseTest("v1.2")(true, None, 1, 2),
              passingParseTest("v1.2.3")(true, None, 1, 2, 3),
              passingParseTest("v1.2.3.4")(true, None, 1, 2, 3, 4),
            ),
            suite("with suffix")(
              passingParseTest("1-SNAPSHOT")(false, Version.Suffix.Snapshot(None).some, 1),
              passingParseTest("1.2-SNAPSHOT")(false, Version.Suffix.Snapshot(None).some, 1, 2),
              passingParseTest("1.2.3-SNAPSHOT")(false, Version.Suffix.Snapshot(None).some, 1, 2, 3),
              passingParseTest("1.2.3.4-SNAPSHOT")(false, Version.Suffix.Snapshot(None).some, 1, 2, 3, 4),
            ),
            suite("with v prefix and suffix")(
              passingParseTest("v1-SNAPSHOT")(true, Version.Suffix.Snapshot(None).some, 1),
              passingParseTest("v1.2-SNAPSHOT")(true, Version.Suffix.Snapshot(None).some, 1, 2),
              passingParseTest("v1.2.3-SNAPSHOT")(true, Version.Suffix.Snapshot(None).some, 1, 2, 3),
              passingParseTest("v1.2.3.4-SNAPSHOT")(true, Version.Suffix.Snapshot(None).some, 1, 2, 3, 4),
            ),
            passingParseTest("v1.2.3.4---SNAPSHOT")(true, Version.Suffix.Other("--SNAPSHOT").some, 1, 2, 3, 4),
          ),
          suite("fails")(
            failingParseTest(" 1.2.3"),
            failingParseTest("1.2.3 "),
            failingParseTest("v 1.2.3"),
            failingParseTest("1.2.3 SNAPSHOT"),
          ),
        ),
        suite("ordering")(
          // without suffix
          orderingTest("0", "1"),
          orderingTest("2", "11"),
          orderingTest("0", "0.1"),
          orderingTest("0.1", "0.1.2"),
          orderingTest("0.1.2", "0.1.2.3"),
          orderingTest("0.0.1", "0.0.2"),
          // with suffix
          orderingTest("0-SNAPSHOT-2", "0-SNAPSHOT-11"),
          orderingTest("0-SNAPSHOT-2", "0-RC1"),
          orderingTest("0-RC2", "0-RC11"),
        ),
        suite("equals")(
          equalsTest("0", "0"),
          equalsTest("3", "3.0.0"),
          equalsTest("v3", "3.0.0"),
        ),
      )

  }

  private object suffixSpec {

    private def passingParseTest(string: String, suffix: Version.Suffix.Repr)(implicit loc: SourceLocation): TestSpec =
      test(string) {
        assertTrue(Version.Suffix.parse(string).repr == suffix)
      }

    private def orderingTest(smaller: String, greater: String)(implicit loc: SourceLocation): TestSpec =
      test(s"$smaller < $greater") {
        val smallerSuffix = Version.Suffix.parse(smaller)
        val greaterSuffix = Version.Suffix.parse(greater)
        assertTrue(
          Version.Suffix.ordering.lt(smallerSuffix, greaterSuffix),
          Version.Suffix.ordering.gt(greaterSuffix, smallerSuffix),
        )
      }

    private def equalsTest(a: String, b: String)(implicit loc: SourceLocation): TestSpec =
      test(s"$a == $b") {
        assertTrue(Version.Suffix.parse(a) == Version.Suffix.parse(b))
      }

    val spec: TestSpec =
      suite("suffix")(
        suite("parse")(
          // snapshot
          passingParseTest("snap", Version.Suffix.Snapshot(None)),
          passingParseTest("snap1", Version.Suffix.Snapshot("1".some)),
          passingParseTest("snap-1", Version.Suffix.Snapshot("-1".some)),
          passingParseTest("snapshot", Version.Suffix.Snapshot(None)),
          passingParseTest("snapshot1", Version.Suffix.Snapshot("1".some)),
          passingParseTest("snapshot-1", Version.Suffix.Snapshot("-1".some)),
          // rc
          passingParseTest("RC1", Version.Suffix.RC(1)),
          passingParseTest("rc2", Version.Suffix.RC(2)),
          passingParseTest("RC-3", Version.Suffix.RC(3)),
          // other
          passingParseTest("snappy", Version.Suffix.Other("SNAPPY")),
          passingParseTest("other", Version.Suffix.Other("OTHER")),
          passingParseTest("RC--3", Version.Suffix.Other("RC--3")),
        ),
        suite("ordering")(
          // same type
          orderingTest("RC1", "RC2"),
          orderingTest("RC2", "RC11"),
          orderingTest("SNAPSHOT", "SNAPSHOT-1"),
          orderingTest("SNAPSHOT-2", "SNAPSHOT-11"),
          orderingTest("SNAPSHOT-2", "SNAPSHOT-11"),
          orderingTest("snappy11", "snappy2"),
          // different type
          orderingTest("SNAPSHOT", "RC1"),
          orderingTest("SNAPSHOT", "snappy"),
          orderingTest("RC1", "snappy"),
        ),
        suite("equals")(
          // snapshot
          equalsTest("snapshot2", "SNAP-2"),
          equalsTest("snapshot-2", "SNAP-2"),
          equalsTest("snapshot2", "SNAP2"),
          equalsTest("snapshot-2", "SNAP2"),
          // rc
          equalsTest("RC1", "rc1"),
          equalsTest("RC1", "rc-1"),
          // other
          equalsTest("snappy", "SNAPPY"),
        ),
      )

  }

  override def testSpec: TestSpec =
    suite("VersionSpec")(
      versionSpec.spec,
      suffixSpec.spec,
    )

}
