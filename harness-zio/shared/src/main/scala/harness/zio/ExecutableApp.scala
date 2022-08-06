package harness.zio

import zio.*

trait ExecutableApp extends ZIOAppDefault {

  val executable: Executable

  override def run: ZIO[ZIOAppArgs with Scope, Any, Any] =
    for {
      args <- ZIOAppArgs.getArgs
      exitCode <- executable(args.toList)
      _ <- exit(exitCode)
    } yield ()

}
