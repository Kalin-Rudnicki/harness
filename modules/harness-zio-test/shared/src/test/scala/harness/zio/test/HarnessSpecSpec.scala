package harness.zio.test

import harness.zio.*
import zio.*
import zio.json.*
import zio.test.*

object HarnessSpecSpec extends HarnessSpec[HarnessSpecSpec.Register] {

  final case class Register(ref: Ref[Set[String]]) {
    def register(name: String): UIO[Unit] = ref.update(_ + name)
    def get: UIO[Set[String]] = ref.get
  }

  override def layerProvider: LayerProvider[R] =
    LayerProvider.provideShared(
      ZLayer.fromZIO { Ref.make(Set.empty[String]).map(Register(_)) },
    )

  private val levels: Chunk[Logger.LogLevel] =
    Chunk(Logger.LogLevel.Error, Logger.LogLevel.Warning, Logger.LogLevel.Important, Logger.LogLevel.Info, Logger.LogLevel.Debug)

  private def makeTest(level: Logger.LogLevel): TestSpec = {
    val label = level.displayName
    test(s"test-$label") {
      for {
        register <- ZIO.service[Register]
        _ <- register.register(label)
        showLevels = Logger.LogLevel.allLevels.filterNot(_ == Logger.LogLevel.Never)
        _ <- ZIO.foreachDiscard(showLevels) { level => Logger.log(level, level.displayName, "test-label" -> label) }
        _ <- Clock.sleep(1.second)

        logs <- LogCache.get
        registered <- register.get

        _ <- Logger.log.info(logs.map(_.toJson).mkString("\n"))
      } yield assertTrue(
        logs.length == showLevels.length,
        logs.forall(_.context.get("test-label").contains(label)),
        registered.size == levels.length,
      )
    } //  @@ HAspects.logger.withLevel(level)
  }

  override def testSpec: TestSpec =
    suite("TmpSpec")(
      levels.map(makeTest) *,
    ) @@
      TestAspect.withLiveClock @@
      TestAspect.parallelN(25) @@
      HAspects.logger.withLevel(Logger.LogLevel.Never)

}
