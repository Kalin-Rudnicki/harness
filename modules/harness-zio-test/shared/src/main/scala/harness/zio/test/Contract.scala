package harness.zio.test

import zio.*
import zio.test.*

abstract class Contract[_R: EnvironmentTag] {

  final type R = _R
  final type DefaultEnv = HarnessSpec.DefaultEnv
  final type Env = DefaultEnv & R

  final type TestSpec = Spec[Env, Any]

  // =====| Abstract |=====

  def testSpec: TestSpec

}
