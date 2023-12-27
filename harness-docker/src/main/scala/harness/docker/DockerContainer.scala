package harness.docker

import cats.syntax.option.*
import harness.core.*

final case class DockerContainer(
    name: String,
    hostName: Option[String],
    image: String,
    imageVersion: String,
    envVars: List[DockerContainer.EnvVar],
    portMappings: List[DockerContainer.Mapping[Int]],
    volumeMappings: List[DockerContainer.Mapping[String]],
    dependsOn: List[String],
) { self =>

  def h: DockerContainer = self.copy(hostName = None)
  def h(hostName: String): DockerContainer = self.copy(hostName = hostName.some)

  def iv(imageVersion: String): DockerContainer = self.copy(imageVersion = imageVersion)
  def iv(imageVersion: Option[String]): DockerContainer = imageVersion.fold(self)(self.iv)

  def e(name: String, value: String): DockerContainer = self.copy(envVars = envVars :+ DockerContainer.EnvVar(name, value))
  def e(name: String, value: Option[String]): DockerContainer = value.fold(self)(self.e(name, _))

  def p(hostPort: Int, containerPort: Int): DockerContainer = self.copy(portMappings = portMappings :+ DockerContainer.Mapping(hostPort, containerPort))
  def p(hostPort: Option[Int], containerPort: Int): DockerContainer = hostPort.fold(self)(self.p(_, containerPort))

  def v(hostPath: String, containerPath: String): DockerContainer = self.copy(volumeMappings = volumeMappings :+ DockerContainer.Mapping(hostPath, containerPath))
  def v(hostPath: Option[String], containerPath: String): DockerContainer = hostPath.fold(self)(self.v(_, containerPath))

  def d(dependsOn: DockerContainer): DockerContainer = self.copy(dependsOn = self.dependsOn :+ dependsOn.name)

  def toYamlString: IndentedString =
    IndentedString.inline(
      s"$name:",
      IndentedString.indented(
        s"image: $image:$imageVersion",
        s"container_name: $name",
        hostName.map { hostName => s"hostname: $hostName" },
        Option.when(dependsOn.nonEmpty) {
          IndentedString.inline(
            "depends_on:",
            IndentedString.indented(
              dependsOn.map(d => s"- $d"),
            ),
          )
        },
        Option.when(portMappings.nonEmpty) {
          IndentedString.inline(
            "ports:",
            IndentedString.indented(
              portMappings.map(p => s"- ${p.host}:${p.container}"),
            ),
          )
        },
        Option.when(volumeMappings.nonEmpty) {
          IndentedString.inline(
            "volumes:",
            IndentedString.indented(
              volumeMappings.map(v => s"- ${v.host}:${v.container}"),
            ),
          )
        },
        Option.when(envVars.nonEmpty) {
          IndentedString.inline(
            "environment:",
            IndentedString.indented(
              envVars.map(e => s"${e.name}: ${e.value}"),
            ),
          )
        },
      ),
    )

}
object DockerContainer {

  def init(name: String, image: String): DockerContainer = DockerContainer(name, name.some, image, "latest", Nil, Nil, Nil, Nil)

  final case class EnvVar(name: String, value: String) {
    def toArgs: List[String] = "-e" :: s"$name=$value" :: Nil
  }

  final case class Mapping[+T](host: T, container: T) {
    def toArgs(flag: String): List[String] = flag :: s"$host:$container" :: Nil
  }

}
