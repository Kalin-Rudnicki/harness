package harness.zio.test

import harness.zio.*
import zio.*
import zio.test.*

abstract class HarnessSpec[_R: EnvironmentTag] extends ZIOSpecDefault {

  final type R = _R
  final type DefaultEnv = HarnessSpec.DefaultEnv
  final type Env = DefaultEnv & R

  final type TestSpecAspect = TestAspectAtLeastR[Env]
  final type TestSpec = Spec[Env, Any]

  // =====| Abstract |=====

  def testSpec: TestSpec

  def layerProvider: LayerProvider[R]

  // =====| Overridable |=====

  def withDefaultAspects: Boolean = true

  /**
    * Chunk of aspects to be applied to [[testSpec]].
    * Note: `Chunk(a, b, c)` will yield `a(b(c(testSpec)))`.
    */
  def testAspects: Chunk[TestSpecAspect] = Chunk.empty

  // TODO (KR) : allow aspects with `Chunk[TestAspectAtLeastR[DefaultEnv]]` that can be applied after layer application.

  // =====| Concrete |=====

  // TODO (KR) : The logger
  override final def spec: Spec[TestEnvironment & Scope, Any] = {
    val spec2: TestSpec =
      testAspects.foldRight(testSpec) { case (a, s) => s @@ a }
    val spec3: Spec[DefaultEnv, Any] =
      layerProvider.build(spec2)
    val spec4: Spec[DefaultEnv, Any] =
      if (withDefaultAspects) HarnessSpec.defaultTestAspects.foldRight(spec3) { case (a, s) => s @@ a }
      else spec3
    val spec5: Spec[TestEnvironment & Scope, Any] =
      spec4.provideSomeLayer(HarnessTestEnv.layer)

    spec5
  }

}
object HarnessSpec {

  type DefaultEnv = HarnessTestEnv & TestEnvironment & Scope

  private val defaultTestAspects: Chunk[TestAspectAtLeastR[DefaultEnv]] =
    Chunk(
      TestAspect.samples(15),
      TestAspect.shrinks(0),
      Logger.withLevel.important.testAspect,
      HAspects.setLoggerSources,
      HAspects.logTestPathAndDuration,
    )

}
