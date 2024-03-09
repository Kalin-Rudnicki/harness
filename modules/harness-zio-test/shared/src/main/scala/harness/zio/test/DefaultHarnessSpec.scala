package harness.zio.test

import harness.zio.*
import zio.*
import zio.test.*

abstract class DefaultHarnessSpec extends HarnessSpec[Any] {
  override final val layer: ULayer[Any] = ZLayer.empty
}
object DefaultHarnessSpec {

  abstract class ForContract[R: EnvironmentTag](name: String, contract: Contract[R])(_layer: ZLayer[HarnessEnv & Scope, Any, R]) extends DefaultHarnessSpec {
    override final def spec: TestSpec = suite(name)(contract.contract).provideSomeLayer[HarnessEnv & TestEnvironment & Scope](_layer)
  }

}
