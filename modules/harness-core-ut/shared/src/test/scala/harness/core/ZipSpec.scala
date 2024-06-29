package harness.core

import harness.zio.test.*
import zio.*
import zio.internal.stacktracer.SourceLocation
import zio.test.*

object ZipSpec extends DefaultHarnessSpec {

  private final class MakeTest[_1, _2, Out](in1: _1, in2: _2)(implicit zip: Zip.Out[_1, _2, Out], loc: SourceLocation) {

    def expect(exp: Out): TestSpec =
      test(s"zip($in1, $in2) -> $exp") {
        val out: zip.Out = zip.zip(in1, in2)
        assertTrue(
          out == exp,
          zip.unzip(out) == (in1, in2),
        )
      }

  }

  private def makeTest[_1, _2](in1: _1, in2: _2)(implicit zip: Zip[_1, _2], loc: SourceLocation): MakeTest[_1, _2, zip.Out] =
    new MakeTest[_1, _2, zip.Out](in1, in2)

  override def testSpec: TestSpec =
    suite("ZipSpec")(
      makeTest(0, ()).expect(0),
      makeTest((), 0).expect(0),
      makeTest(1, 2).expect((1, 2)),
      makeTest((1, 2), 3).expect((1, 2, 3)),
      makeTest((1, 2, 3), 4).expect((1, 2, 3, 4)),
      makeTest((1, 2, 3, 4), 5).expect((1, 2, 3, 4, 5)),
      makeTest(1, (2, 3)).expect((1, 2, 3)),
      makeTest(1, (2, 3, 4)).expect((1, 2, 3, 4)),
      makeTest(1, (2, 3, 4, 5)).expect((1, 2, 3, 4, 5)),
      makeTest((1, 2), (3, 4)).expect((1, 2, 3, 4)),
      makeTest((1, 2, 3), (4, 5, 6)).expect((1, 2, 3, 4, 5, 6)),
      makeTest((1, 2, 3, 4), (5, 6, 7, 8)).expect((1, 2, 3, 4, 5, 6, 7, 8)),
    )

}
