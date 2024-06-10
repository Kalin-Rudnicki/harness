package harness.endpoint.typeclass

import harness.endpoint.*
import harness.endpoint.spec.{EndpointSpec, PathCodec}
import harness.endpoint.types.EndpointType

trait PathPrepend[T[_[_ <: EndpointType.Any]]] {
  def prepend(path: String, t: T[EndpointSpec]): T[EndpointSpec]
}
object PathPrepend {

  implicit def id[ET <: EndpointType.Any]: PathPrepend[K11ET.Id[ET]] =
    new PathPrepend[K11ET.Id[ET]] {
      override def prepend(path: String, t: K11ET.Id[ET][EndpointSpec]): K11ET.Id[ET][EndpointSpec] =
        t.copy(pathCodec = PathCodec.Const(path) / t.pathCodec)
    }

  inline implicit def genProduct[T[_[_ <: EndpointType.Any]]](implicit m: K11ET.ProductGeneric[T]): PathPrepend[T] = {
    val inst = K11ET.ProductInstances.of[T, PathPrepend]

    new PathPrepend[T] {
      override def prepend(path: String, t: T[EndpointSpec]): T[EndpointSpec] =
        inst.withInstance(t).mapInstantiate { [t[_[_ <: EndpointType.Any]]] => (pp: PathPrepend[t], i: t[EndpointSpec]) => pp.prepend(path, i) }
    }
  }

}

extension (self: String) {
  def /:[T[_[_ <: EndpointType.Any]]](t: T[EndpointSpec])(implicit pp: PathPrepend[T]): T[EndpointSpec] =
    pp.prepend(self, t)
}
