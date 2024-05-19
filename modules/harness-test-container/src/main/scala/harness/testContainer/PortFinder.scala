package harness.testContainer

import cats.syntax.option.*
import harness.zio.*
import java.net.InetSocketAddress
import java.net.Socket
import zio.*

final class PortFinder(min: Int, max: Int, acquiredRef: Ref.Synchronized[Set[Int]], random: Option[Random]) {

  private val randomPortList: UIO[List[Int]] = {
    val effect = Random.shuffle(Range.inclusive(min, max).toList)
    random.fold(effect)(effect.withRandom)
  }

  private def attemptSocketConnect(host: String, port: Int): ZIO[Logger, Int, Int] =
    ZIO.scoped {
      Logger.addContext("target-port" -> port) {
        Logger.log.debug("Attempting to acquire port") *>
          ZIO
            .attempt { new Socket() }
            .withFinalizer { socket => ZIO.attempt { socket.close() }.ignore }
            .tap { socket => ZIO.attempt { socket.connect(new InetSocketAddress(host, port)) } }
            .foldZIO(
              _ => ZIO.succeed(port),
              _ => ZIO.fail(port),
            ) <*
          Logger.log.debug("Acquired port")
      }
    }

  private def attemptConnect(host: String, port: Int): ZIO[Logger & Scope, Int, Int] =
    acquiredRef.modifyZIO { acquired =>
      if (acquired.contains(port)) ZIO.fail(port)
      else attemptSocketConnect(host, port).map(_ -> (acquired + port))
    } <*
      ZIO.addFinalizer(acquiredRef.update(_ - port))

  private def rec(host: String, containerName: String, ports: List[Int]): URIO[Logger & Scope, Int] =
    ports match {
      case port :: rest =>
        attemptConnect(host, port).foldCauseZIO(
          _ => rec(host, containerName, rest),
          ZIO.succeed(_),
        )
      case Nil =>
        ZIO.dieMessage(s"Unable to acquire port for container '$containerName' in range $min-$max")
    }

  def acquirePort(containerName: String): URIO[Logger & Scope, Int] =
    Logger.addContext("container-name" -> containerName) {
      Logger.log.debug(s"Attempting to acquire localhost port for container '$containerName' in range [$min, $max]") *>
        randomPortList.flatMap(rec("localhost", containerName, _))
    }

}
object PortFinder {

  def layer(min: Int = 6100, max: Int = 6250, random: Option[Random] = Random.RandomLive.some): ULayer[PortFinder] =
    ZLayer.fromZIO {
      Ref.Synchronized.make(Set.empty[Int]).map { new PortFinder(min, max, _, random) }
    }

  def acquirePort(containerName: String): URIO[PortFinder & Logger & Scope, Int] =
    ZIO.serviceWithZIO[PortFinder](_.acquirePort(containerName))

}
