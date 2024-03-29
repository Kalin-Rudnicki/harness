package harness.test

import zio.*
import zio.test.*

abstract class PlainHarnessSpec extends ZIOSpecDefault {

  final type TestSpec = Spec[TestEnvironment & Scope, Any]

  override def spec: TestSpec

  override def aspects: Chunk[TestAspectAtLeastR[TestEnvironment]] =
    Chunk(TestAspect.samples(15), TestAspect.shrinks(0))

}
