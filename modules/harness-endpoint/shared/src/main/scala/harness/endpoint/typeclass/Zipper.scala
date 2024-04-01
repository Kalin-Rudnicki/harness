package harness.endpoint.typeclass

import harness.endpoint.*
import harness.endpoint.types.EndpointType

trait Zipper[T[_[_ <: EndpointType.Any]]] {
  def zip[A[_ <: EndpointType.Any], B[_ <: EndpointType.Any]](a: T[A], b: T[B]): T[K11ET.Zip[A, B]]
}
object Zipper {

  def zip[T[_[_ <: EndpointType.Any]], A[_ <: EndpointType.Any], B[_ <: EndpointType.Any]](a: T[A], b: T[B])(implicit f: Zipper[T]): T[K11ET.Zip[A, B]] =
    f.zip(a, b)

  implicit def id[ET <: EndpointType.Any]: Zipper[K11ET.Id[ET]] =
    new Zipper[K11ET.Id[ET]] {
      override def zip[A[_ <: EndpointType.Any], B[_ <: EndpointType.Any]](a: K11ET.Id[ET][A], b: K11ET.Id[ET][B]): (A[ET], B[ET]) =
        (a, b)
    }

  inline implicit def genProduct[T[_[_ <: EndpointType.Any]]](implicit m: K11ET.ProductGeneric[T]): Zipper[T] = {
    val inst = K11ET.ProductInstances.of[T, Zipper]

    new Zipper[T] {
      override def zip[A[_ <: EndpointType.Any], B[_ <: EndpointType.Any]](a: T[A], b: T[B]): T[K11ET.Zip[A, B]] =
        inst.withInstance2(a, b).mapInstantiate { [t[_[_ <: EndpointType.Any]]] => (i: Zipper[t], a: t[A], b: t[B]) => i.zip(a, b) }
    }
  }

}
