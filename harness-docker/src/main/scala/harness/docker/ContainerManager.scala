package harness.docker

import harness.cli.*
import harness.core.HError
import harness.zio.*
import zio.*

final case class ContainerManager[R](
    desiredContainers: SHRIO[R, List[DockerContainer]],
) { self =>

  def ++[R2](other: ContainerManager[R2]): ContainerManager[R & R2] =
    ContainerManager[R & R2](
      (self.desiredContainers <*> other.desiredContainers).map(_ ++ _),
    )

  def toExecutable(layer: SHTaskLayer[R & ContainerManager.BaseEnv])(implicit environmentTag: EnvironmentTag[R & ContainerManager.BaseEnv]): Executable =
    Executable.fromSubCommands(
      "start" ->
        Executable
          .withParser { ContainerManager.StartConfig.parser }
          .withLayer { layer }
          .withEffect { config =>
            for {
              _ <- Logger.log.info(s"Running docker start, with auto-stop=${config.autoStop}")
              runningContainers <- DockerCommands.getContainers
              desiredContainers <- self.desiredContainers

              _ <- Logger.log.debug(s"Desired containers:${desiredContainers.map(c => s"\n  - ${c.name}").mkString}")

              runningNameMap = runningContainers.map(c => (c.Names, c)).toMap
              runningDesired = desiredContainers.flatMap(c => runningNameMap.get(c.name)).toSet

              _ <-
                if (runningDesired.nonEmpty && !config.autoStop) ZIO.fail(HError.UserError(s"Containers are already running: ${runningDesired.map(_.Names).mkString("[", ", ", "]")}"))
                else ZIO.foreachDiscard(runningDesired)(DockerCommands.stopAndRemoveContainer)

              _ <- ZIO.foreachDiscard(desiredContainers)(DockerCommands.runContainer)
            } yield ()
          },
      "stop" ->
        Executable
          .withLayer { layer }
          .withEffect {
            for {
              _ <- Logger.log.info(s"Running docker stop")
              runningContainers <- DockerCommands.getContainers
              desiredContainers <- self.desiredContainers

              _ <- Logger.log.debug(s"Desired containers:${desiredContainers.map(c => s"\n  - ${c.name}").mkString}")

              runningNameMap = runningContainers.map(c => (c.Names, c)).toMap
              runningDesired = desiredContainers.flatMap(c => runningNameMap.get(c.name)).toSet

              _ <- ZIO.foreachDiscard(runningDesired)(DockerCommands.stopAndRemoveContainer)
            } yield ()
          },
    )

}
object ContainerManager {

  type BaseEnv = DockerNeedsSudo

  final case class StartConfig(
      autoStop: Boolean,
  )
  object StartConfig {
    val parser: Parser[StartConfig] =
      (
        Parser.flag(LongName.unsafe("auto-stop"), helpHint = List("if there is a container already running, stop it")),
      ).map(StartConfig.apply)
  }

  val empty: ContainerManager[Any] =
    ContainerManager[Any](ZIO.succeed(Nil))

}
