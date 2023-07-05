package harness.archive.client

import zio.*

private[client] object Util {

  def logToConsole(str: => String): UIO[Unit] =
    ZIO.succeed { println(str) }

}
