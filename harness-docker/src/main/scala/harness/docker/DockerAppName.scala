package harness.docker

import harness.zio.*
import zio.*

final case class DockerAppName(value: String)
object DockerAppName {

  val value: URIO[DockerAppName, String] = ZIO.serviceWith[DockerAppName](_.value)

  def layer(value: String): ULayer[DockerAppName] = ZLayer.succeed(DockerAppName(value))

  def configLayer(jsonPath: String*): HRLayer[HConfig, DockerAppName] =
    HConfig.readLayer[String](jsonPath*).project(DockerAppName(_))

}
