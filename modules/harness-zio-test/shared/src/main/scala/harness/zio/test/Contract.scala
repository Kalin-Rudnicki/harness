package harness.zio.test

import harness.zio.*
import zio.*
import zio.test.*

abstract class Contract[R: EnvironmentTag] {

  final type TestSpec = Spec[HarnessEnv & R & TestEnvironment & Scope, Any]

  def contract: TestSpec

}
