package harness.endpoint.typeclass

import harness.endpoint.*
import harness.endpoint.typeclass.K11ET.~>
import harness.endpoint.types.EndpointType

trait MapK[T[_[_ <: EndpointType.Any]]] {
  def mapK[A[_ <: EndpointType.Any], B[_ <: EndpointType.Any]](a: T[A])(f: A ~> B): T[B]
}
object MapK {

  def mapK[T[_[_ <: EndpointType.Any]], A[_ <: EndpointType.Any], B[_ <: EndpointType.Any]](a: T[A])(f: A ~> B)(implicit m: MapK[T]): T[B] =
    m.mapK(a)(f)

  implicit def id[ET <: EndpointType.Any]: MapK[K11ET.Id[ET]] =
    new MapK[K11ET.Id[ET]] {
      override def mapK[A[_ <: EndpointType.Any], B[_ <: EndpointType.Any]](a: K11ET.Id[ET][A])(f: A ~> B): K11ET.Id[ET][B] = f(a)
    }

  inline implicit def genProduct[T[_[_ <: EndpointType.Any]]](implicit m: K11ET.ProductGeneric[T]): MapK[T] = {
    val inst = K11ET.ProductInstances.of[T, MapK]

    new MapK[T] {
      override def mapK[A[_ <: EndpointType.Any], B[_ <: EndpointType.Any]](a: T[A])(f: A ~> B): T[B] =
        inst.withInstance(a).mapInstantiate { [t[_[_ <: EndpointType.Any]]] => (i: MapK[t], t: t[A]) => i.mapK(t)(f) }
    }
  }

}
