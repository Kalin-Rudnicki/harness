package harness.docker

import harness.zio.*
import zio.*

final case class DockerNeedsSudo(value: Boolean)
object DockerNeedsSudo {

  val value: URIO[DockerNeedsSudo, Boolean] = ZIO.serviceWith[DockerNeedsSudo](_.value)

  def layer(value: Boolean): ULayer[DockerNeedsSudo] = ZLayer.succeed(DockerNeedsSudo(value))

  def configLayer(jsonPath: String*): HRLayer[Config, DockerNeedsSudo] =
    Config.readLayer[Boolean](jsonPath*).project(DockerNeedsSudo(_))

}
