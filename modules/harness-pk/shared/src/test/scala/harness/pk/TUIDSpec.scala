package harness.pk

import harness.zio.test.DefaultHarnessSpec
import java.time.Instant
import zio.test.*

object TUIDSpec extends DefaultHarnessSpec {

  private def makeTest(name: String)(instant: Instant, highBytes: Long, lowBytes: Long)(exp: String): TestSpec =
    test(name) {
      assertTrue(TUID.make(instant, highBytes, lowBytes).toUUID.toString.replaceAll("-", "") == exp.replaceAll("-", ""))
    }

  override def testSpec: TestSpec =
    suite("TUIDSpec")(
      suite("epoch 0")(
        makeTest("0/0")(Instant.ofEpochSecond(0), 0L, 0L)("000000000-0000000-0000000000000000"),
        makeTest("-1/0")(Instant.ofEpochSecond(0), -1L, 0L)("000000000-fffffff-0000000000000000"),
        makeTest("0/-1")(Instant.ofEpochSecond(0), 0, -1L)("000000000-0000000-ffffffffffffffff"),
        makeTest("-1/-1")(Instant.ofEpochSecond(0), -1L, -1L)("000000000-fffffff-ffffffffffffffff"),
      ),
      suite("epoch(2 ^ 36) - 1")(
        makeTest("0/0")(Instant.ofEpochSecond(Math.pow(2, 36).toLong - 1), 0L, 0L)("fffffffff-0000000-0000000000000000"),
        makeTest("-1/0")(Instant.ofEpochSecond(Math.pow(2, 36).toLong - 1), -1L, 0L)("fffffffff-fffffff-0000000000000000"),
        makeTest("0/-1")(Instant.ofEpochSecond(Math.pow(2, 36).toLong - 1), 0, -1L)("fffffffff-0000000-ffffffffffffffff"),
        makeTest("-1/-1")(Instant.ofEpochSecond(Math.pow(2, 36).toLong - 1), -1L, -1L)("fffffffff-fffffff-ffffffffffffffff"),
      ),
      suite("epoch(2 ^ 36)")(
        makeTest("0/0")(Instant.ofEpochSecond(Math.pow(2, 36).toLong), 0L, 0L)("000000000-0000000-0000000000000000"),
        makeTest("-1/0")(Instant.ofEpochSecond(Math.pow(2, 36).toLong), -1L, 0L)("000000000-fffffff-0000000000000000"),
        makeTest("0/-1")(Instant.ofEpochSecond(Math.pow(2, 36).toLong), 0, -1L)("000000000-0000000-ffffffffffffffff"),
        makeTest("-1/-1")(Instant.ofEpochSecond(Math.pow(2, 36).toLong), -1L, -1L)("000000000-fffffff-ffffffffffffffff"),
      ),
    )

}
