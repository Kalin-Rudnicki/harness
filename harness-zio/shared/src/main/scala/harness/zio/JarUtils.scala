package harness.zio

import harness.core.*
import java.io.InputStream
import zio.*

object JarUtils {

  def findInputStream(path: String): HRIO[Scope, Option[InputStream]] =
    ZIO
      .hAttempt { Option(this.getClass.getClassLoader.getResourceAsStream(path)) }
      .withFinalizer(ZIO.foreachDiscard(_) { stream => ZIO.attempt { stream.close() }.orDie })

  def getInputStream(path: String): HRIO[Scope, InputStream] =
    findInputStream(path).someOrFail(HError.InternalDefect(s"No such jar resource: $path"))

}
