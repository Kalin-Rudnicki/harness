package harness.zio.test

import harness.zio.*
import zio.*
import zio.test.*

abstract class HarnessSpec[_R: EnvironmentTag] extends ZIOSpecDefault {

  final type R = _R
  final type DefaultEnv = HarnessSpec.DefaultEnv
  final type Env = DefaultEnv & R

  final type TestSpec = Spec[Env, Any]

  // =====| Abstract |=====

  def testSpec: TestSpec

  def layerProvider: LayerProvider[R]

  // =====| Overridable |=====

  def withDefaultAspects: Boolean = true

  def testAspects: Chunk[TestAspectAtLeastR[Env]] = Chunk.empty

  // =====| Concrete |=====

  override final def spec: Spec[TestEnvironment & Scope, Any] = {
    val allAspects: Chunk[TestAspectAtLeastR[Env]] =
      if (withDefaultAspects) HarnessSpec.defaultTestAspects ++ testAspects
      else testAspects
    val spec2: TestSpec = allAspects.foldLeft(testSpec)(_ @@ _)
    val spec3: Spec[DefaultEnv, Any] = layerProvider.build(spec2)
    val spec4: Spec[TestEnvironment & Scope, Any] = spec3.provideSomeLayer(HarnessTestEnv.layer)

    spec4
  }

}
object HarnessSpec {

  type DefaultEnv = HarnessEnv & HarnessTestEnv & TestEnvironment & Scope

  private val defaultTestAspects: Chunk[TestAspectAtLeastR[DefaultEnv]] =
    Chunk(TestAspect.samples(15), TestAspect.shrinks(0), HAspects.logTestPathAndDuration)

}
