package harness.zio.test

import harness.zio.*
import zio.*
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
    val label = level.rawDisplayName
    test(s"test-$label") {
      for {
        register <- ZIO.service[Register]
        _ <- register.register(label)
        showLevels = Logger.LogLevel.enumValues.filterNot(_ == Logger.LogLevel.Never)
        _ <- ZIO.foreachDiscard(showLevels) { level => Logger.log(level)(label, "test-label" -> label) }
        _ <- Clock.sleep(1.second)

        traceLogs <- LogCache.getTrace
        defaultLogs <- LogCache.getDefault
        registered <- register.get

        // logger <- Logger.current
        // _ <- Console.printLine {
        //   import zio.json.*
        //   s"""$logger
        //      |  trace-events:${traceLogs.map { l => s"\n    - ${l.toJson}" }.mkString}
        //      |  default-events:${defaultLogs.map { l => s"\n    - ${l.toJson}" }.mkString}""".stripMargin
        // }
      } yield assertTrue(
        traceLogs.length == showLevels.length,
        traceLogs.filterNot(_.context.toMap.get("test-label").contains(label)).isEmpty,
        defaultLogs.length == showLevels.count(_.logPriority >= level.tolerancePriority),
        defaultLogs.filterNot(_.context.toMap.get("test-label").contains(label)).isEmpty,
        registered.contains(label),
        registered.size == levels.length,
      )
    } @@ Logger.withLevel(level).testAspect

  }

  override def testAspects: Chunk[TestSpecAspect] =
    Chunk(
      TestAspect.withLiveClock,
      TestAspect.parallelN(25),
      Logger.withLevel.never.testAspect,
      Logger.withoutStdOutSources.testAspect,
    )

  override def testSpec: TestSpec =
    suite("TmpSpec")(
      levels.map(makeTest)*,
    )

}
