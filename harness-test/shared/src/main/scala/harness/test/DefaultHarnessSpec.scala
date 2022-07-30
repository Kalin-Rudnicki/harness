package harness.test

import zio.*
import zio.test.*

abstract class DefaultHarnessSpec extends ZIOSpecDefault {

  final type TestSpec = Spec[TestEnvironment with Scope, Any]

  override def aspects: Chunk[TestAspectAtLeastR[TestEnvironment]] = Chunk(TestAspect.shrinks(0))

}
