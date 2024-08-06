package harness.testContainer

import zio.*

final case class TestContainer(
    name: String,
    imageName: String,
    imageVersion: String,
    healthCheck: Task[Boolean],
    envVars: List[TestContainer.Var],
    ports: List[TestContainer.PortMapping],
    labels: List[TestContainer.Var],
) { self =>

  def e(varName: String, varValue: String): TestContainer = self.copy(envVars = self.envVars :+ TestContainer.Var(varName, varValue))
  def e(varName: String, varValue: Option[String]): TestContainer = varValue.fold(self)(self.e(varName, _))

  def p(localPort: Int, containerPort: Int): TestContainer = self.copy(ports = self.ports :+ TestContainer.PortMapping(localPort, containerPort))
  def p(localPort: Option[Int], containerPort: Int): TestContainer = localPort.fold(self)(self.p(_, containerPort))
  def p(localPort: Int, containerPort: Option[Int]): TestContainer = containerPort.fold(self)(self.p(localPort, _))
  def p(localPort: Option[Int], containerPort: Option[Int]): TestContainer = (for { l <- localPort; c <- containerPort } yield self.p(l, c)).getOrElse(self)

  def l(varName: String, varValue: String): TestContainer = self.copy(labels = self.labels :+ TestContainer.Var(varName, varValue))
  def l(varName: String, varValue: Option[String]): TestContainer = varValue.fold(self)(self.l(varName, _))

}
object TestContainer {

  final case class Var(varName: String, varValue: String) {
    override def toString: String = s"$varName=$varValue"
  }
  final case class PortMapping(local: Int, container: Int) {
    override def toString: String = s"$local:$container"
  }

}
