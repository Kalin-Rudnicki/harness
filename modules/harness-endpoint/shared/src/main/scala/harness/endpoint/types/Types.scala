package harness.endpoint.types

import harness.endpoint.transfer.*

object Types {

  type Path[ET <: EndpointType.Any] = ET match {
    case EndpointType[t, ?, ?, ?, ?, ?, ?] => t
  }

  type Query[ET <: EndpointType.Any] = ET match {
    case EndpointType[?, t, ?, ?, ?, ?, ?] => t
  }

  type Auth[ET <: EndpointType.Any] = ET match {
    case EndpointType[?, ?, t, ?, ?, ?, ?] => t
  }

  type Header[ET <: EndpointType.Any] = ET match {
    case EndpointType[?, ?, ?, t, ?, ?, ?] => t
  }

  type InputBody[ET <: EndpointType.Any] <: BodyType = ET match {
    case EndpointType[?, ?, ?, ?, t, ?, ?] => t
  }

  type OutputBody[ET <: EndpointType.Any] <: BodyType = ET match {
    case EndpointType[?, ?, ?, ?, ?, t, ?] => t
  }

  type Error[ET <: EndpointType.Any] = ET match {
    case EndpointType[?, ?, ?, ?, ?, ?, t] => t
  }

  type Receive[B <: BodyType] = B match {
    case BodyType.Encoded[o] => o
    case BodyType.Stream     => InputStream
    case BodyType.None       => Unit
  }

  type Send[B <: BodyType] = B match {
    case BodyType.Encoded[o] => o
    case BodyType.Stream     => OutputStream
    case BodyType.None       => Unit
  }

}
