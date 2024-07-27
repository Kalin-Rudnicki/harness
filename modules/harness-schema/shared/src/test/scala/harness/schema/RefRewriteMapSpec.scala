package harness.schema

import harness.zio.test.*
import java.util.UUID
import zio.test.*

object RefRewriteMapSpec extends DefaultHarnessSpec {

  private def uuid(i: Int): UUID = UUID(0, i)
  private def convertMap(map: Map[Int, Int]): Map[UUID, UUID] = map.map { case (k, v) => (uuid(k), uuid(v)) }

  private def makeTest(name: String)(leftMap: Map[Int, Int], rightMap: Map[Int, Int], expMap: Map[Int, Int]): TestSpec =
    test(name) {
      assertTrue(
        (SchemaSpecs.RefRewriteMap(convertMap(leftMap)) ++ SchemaSpecs.RefRewriteMap(convertMap(rightMap))).map == convertMap(expMap),
      )
    }

  override def testSpec: TestSpec =
    suite("RefRewriteMapSpec")(
      makeTest("empty")(
        Map(),
        Map(),
        Map(),
      ),
      makeTest("disjoint")(
        Map(1 -> 2),
        Map(3 -> 4),
        Map(1 -> 2, 3 -> 4),
      ),
      makeTest("right overrides left")(
        Map(1 -> 2),
        Map(1 -> 3),
        Map(1 -> 3),
      ),
      makeTest("left dereferences right")(
        Map(1 -> 2),
        Map(2 -> 3),
        Map(1 -> 3, 2 -> 3),
      ),
      makeTest("right dereferences left")(
        Map(2 -> 3),
        Map(1 -> 2),
        Map(1 -> 3, 2 -> 3),
      ),
      makeTest("dereferences in both")(
        Map(11 -> 12, 22 -> 23),
        Map(12 -> 13, 21 -> 22),
        Map(11 -> 13, 12 -> 13, 21 -> 23, 22 -> 23),
      ),
      makeTest("override and dereference play nicely (1)")(
        Map(1 -> 2),
        Map(1 -> 3, 2 -> 4),
        Map(1 -> 3, 2 -> 4),
      ),
      makeTest("override and dereference play nicely (2)")(
        Map(1 -> 2, 4 -> 5),
        Map(1 -> 3, 2 -> 4),
        Map(1 -> 3, 2 -> 5, 4 -> 5),
      ),
    )

}
