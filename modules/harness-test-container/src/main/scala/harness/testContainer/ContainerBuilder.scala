package harness.testContainer

import cats.syntax.option.*
import harness.zio.*
import java.util.UUID
import zio.*
import zio.internal.stacktracer.SourceLocation

abstract class ContainerBuilder[RIn, ROut](final val containerType: String) { self =>

  protected type Internal

  protected def makeInternal(metadata: ContainerBuilder.Metadata): RIO[HarnessEnv & RIn & Scope, Internal]
  protected def makeContainers(internal: Internal, metadata: ContainerBuilder.Metadata): RIO[HarnessEnv & RIn & Scope, List[TestContainer]]
  protected def makeEnv(internal: Internal, metadata: ContainerBuilder.Metadata): RIO[HarnessEnv & RIn & Scope, ZEnvironment[ROut]]

  private def startContainer(container: TestContainer): RIO[HarnessEnv & Scope, Unit] = {
    val runCommand = Sys.Command(
      "docker",
      List("run", "-d", "--name", container.name).some,
      container.envVars.flatMap(e => List("-e", e.toString)).some,
      container.ports.flatMap(p => List("-p", p.toString)).some,
      container.labels.flatMap(l => List("-l", l.toString)).some,
      List(s"${container.imageName}:${container.imageVersion}").some,
    )
    val runEffect: RIO[Logger, Unit] =
      Logger.log.detailed(s"Starting container '${container.name}'") *>
        Sys.execute0.runComplex(outLevel = Logger.LogLevel.Detailed)(runCommand)
    val closeEffect: RIO[Logger, Unit] =
      Logger.log.detailed(s"Stopping container '${container.name}'") *>
        Sys.execute0.runComplex(outLevel = Logger.LogLevel.Trace)(Sys.Command("docker", "stop", container.name)) *>
        Sys.execute0.runComplex(outLevel = Logger.LogLevel.Trace)(Sys.Command("docker", "rm", container.name))

    runEffect.withFinalizer { _ => closeEffect.orDie }
  }

  private def awaitContainers(containers: List[TestContainer], waitIncrement: Duration, waited: Duration, maxWait: Duration): RIO[HarnessEnv, Unit] =
    for {
      _ <- ZIO.fail(new RuntimeException("Timed out waiting for containers")).when(waited >= maxWait)
      awaiting <- ZIO.filterNot(containers) { _.healthCheck.orElseSucceed(false) }
      _ <- ZIO.when(awaiting.nonEmpty) {
        Logger.log.debug(s"Awaiting container(s): ${awaiting.map { c => s"'${c.name}'" }.mkString(", ")}") *>
          Clock.sleep(waitIncrement).withClock(Clock.ClockLive) *>
          awaitContainers(awaiting, waitIncrement, waited + waitIncrement, maxWait)
      }
    } yield ()

  final def layer(implicit loc: SourceLocation): RLayer[HarnessEnv & RIn, ROut] =
    ZLayer.scopedEnvironment {
      val metadata = ContainerBuilder.Metadata(containerType, loc, UUID.randomUUID)

      Logger.addContext("container-type" -> metadata.containerType, "layer-id" -> metadata.layerId) {
        for {
          _ <- Logger.log.detailed(s"Creating test-container layer for '$containerType'")
          internal <- self.makeInternal(metadata)
          containers <- self.makeContainers(internal, metadata)
          _ <- ZIO.foreachDiscard(containers)(self.startContainer)
          _ <- awaitContainers(containers, 100.millis, Duration.Zero, 5.seconds)
          env <- makeEnv(internal, metadata)
        } yield env
      }
    }

}
object ContainerBuilder {

  final case class Metadata(containerType: String, loc: SourceLocation, layerId: UUID) {

    def makeContainer(containerName: String, imageName: String, imageVersion: String)(healthCheck: RIO[HarnessEnv, Boolean]): TestContainer =
      TestContainer(
        name = s"$containerType--$containerName--$layerId",
        imageName = imageName,
        imageVersion = imageVersion,
        healthCheck = healthCheck,
        envVars = Nil,
        ports = Nil,
        labels = List(
          TestContainer.Var("HARNESS-TEST-CONTAINER--CONTAINER-TYPE", containerType),
          TestContainer.Var("HARNESS-TEST-CONTAINER--SOURCE-PATH", loc.path),
          TestContainer.Var("HARNESS-TEST-CONTAINER--SOURCE-LINE", loc.line.toString),
          TestContainer.Var("HARNESS-TEST-CONTAINER--LAYER-ID", layerId.toString),
        ),
      )

  }

}
