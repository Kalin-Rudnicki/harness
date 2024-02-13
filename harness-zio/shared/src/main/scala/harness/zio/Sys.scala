package harness.zio

import cats.data.NonEmptyList
import cats.syntax.option.*
import harness.core.*
import scala.sys.process.*
import zio.*

object Sys {

  final case class HarnessProcessLogger(runtime: Runtime[Logger], outLevel: Logger.LogLevel, errLevel: Logger.LogLevel) extends ProcessLogger {
    override def out(s: => String): Unit =
      Unsafe.unsafely { runtime.unsafe.run(Logger.log(outLevel, s)).getOrThrow() }

    override def err(s: => String): Unit =
      Unsafe.unsafely { runtime.unsafe.run(Logger.log(errLevel, s)).getOrThrow() }

    override def buffer[T](f: => T): T = f
  }

  final case class Command(cmdAndArgs: NonEmptyList[String]) {

    def sudo: Command = Command("sudo" :: cmdAndArgs)
    def sudoIf(cond: Boolean): Command = if (cond) this.sudo else this
    def :+(tail: List[String]): Command = Command(cmdAndArgs ++ tail)

    def show: String = cmdAndArgs.toList.map(_.unesc).mkString(" ")

  }
  object Command {
    def apply(cmd: String, args: String*): Command =
      new Command(NonEmptyList(cmd, args.toList))
    def apply(cmd: String, arg0: Option[List[String]], argN: Option[List[String]]*): Command =
      new Command(NonEmptyList(cmd, (arg0 :: argN.toList).flatMap(_.toList.flatten)))
  }

  abstract class CmdBuilder[O](private[Sys] val run: (ProcessBuilder, Option[HarnessProcessLogger], Command) => HTask[O]) { self =>

    private final def makeRun(envVars: List[(String, String)], logger: Option[HarnessProcessLogger], cmd: Command): HTask[O] =
      self.run(Process(cmd.cmdAndArgs.toList, None, envVars*), logger, cmd)

    private final def makeLogger(outLevel: Logger.LogLevel, errLevel: Logger.LogLevel): URIO[Logger, HarnessProcessLogger] =
      ZIO.runtime[Logger].map(HarnessProcessLogger(_, outLevel, errLevel))

    final def runComplex(
        envVars: Map[String, String] = Map.empty,
        outLevel: Logger.LogLevel = Logger.LogLevel.Info,
        errLevel: Logger.LogLevel = Logger.LogLevel.Error,
    )(cmd: Command): HRIO[Logger, O] =
      for {
        logger <- makeLogger(outLevel, errLevel)
        res <- makeRun(envVars.toList, logger.some, cmd)
      } yield res

    final def runSimple(cmd: Command): HTask[O] = makeRun(Nil, None, cmd)
    final def runSimple(cmdAndArgs: NonEmptyList[String]): HTask[O] = runSimple(Command(cmdAndArgs))
    final def runSimple(cmd: String, args: String*): HTask[O] = runSimple(Command(NonEmptyList(cmd, args.toList)))

  }

  private def doRun[O](process: ProcessBuilder, logger: Option[HarnessProcessLogger], command: Command)(
      withLogger: (ProcessBuilder, HarnessProcessLogger) => O,
      withoutLogger: ProcessBuilder => O,
  ): HTask[O] =
    ZIO
      .hAttempt {
        logger match {
          case Some(logger) => withLogger(process, logger)
          case None         => withoutLogger(process)
        }
      }
      .mapError(HError.SystemFailure(s"Unable to run command: ${command.show}", _))

  object execute extends CmdBuilder[Int](doRun(_, _, _)(_.!<(_), _.!<))

  object execute0
      extends CmdBuilder[Unit]((process, logger, cmdAndArgs) =>
        Sys.execute
          .run(process, logger, cmdAndArgs)
          .flatMap {
            case 0    => ZIO.unit
            case code => ZIO.fail(HError.SystemFailure(s"Sys command exited with non-0 code '$code': ${cmdAndArgs.show}"))
          },
      )

  object executeString0 extends CmdBuilder[String](doRun(_, _, _)(_.!!<(_), _.!!<))

}
