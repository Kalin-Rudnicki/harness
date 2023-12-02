package harness.docker

final case class DockerContainer(
    name: String,
    image: String,
    envVars: List[DockerContainer.EnvVar],
    portMappings: List[DockerContainer.Mapping[Int]],
    volumeMappings: List[DockerContainer.Mapping[String]],
) { self =>

  def e(name: String, value: String): DockerContainer = self.copy(envVars = envVars :+ DockerContainer.EnvVar(name, value))
  def e(name: String, value: Option[String]): DockerContainer = value.fold(self)(self.e(name, _))

  def p(hostPort: Int, containerPort: Int): DockerContainer = self.copy(portMappings = portMappings :+ DockerContainer.Mapping(hostPort, containerPort))
  def p(hostPort: Option[Int], containerPort: Int): DockerContainer = hostPort.fold(self)(self.p(_, containerPort))

  def v(hostPath: String, containerPath: String): DockerContainer = self.copy(volumeMappings = volumeMappings :+ DockerContainer.Mapping(hostPath, containerPath))
  def v(hostPath: Option[String], containerPath: String): DockerContainer = hostPath.fold(self)(self.v(_, containerPath))

}
object DockerContainer {

  def init(name: String, image: String): DockerContainer = DockerContainer(name, image, Nil, Nil, Nil)

  final case class EnvVar(name: String, value: String) {
    def toArgs: List[String] = "-e" :: s"$name=$value" :: Nil
  }

  final case class Mapping[+T](host: T, container: T) {
    def toArgs(flag: String): List[String] = flag :: s"$host:$container" :: Nil
  }

}
