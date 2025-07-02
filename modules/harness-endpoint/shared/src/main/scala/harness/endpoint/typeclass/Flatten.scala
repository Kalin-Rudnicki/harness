package harness.endpoint.typeclass

import harness.endpoint.*
import harness.endpoint.types.EndpointType

trait Flatten[T[_[_ <: EndpointType.Any]]] {
  def flatten[F[_ <: EndpointType.Any]](t: T[F]): List[F[EndpointType.Any]]
}
object Flatten {

  def flatten[T[_[_ <: EndpointType.Any]], F[_ <: EndpointType.Any]](t: T[F])(implicit f: Flatten[T]): List[F[EndpointType.Any]] =
    f.flatten(t)

  implicit def id[ET <: EndpointType.Any]: Flatten[K11ET.Id[ET]] =
    new Flatten[K11ET.Id[ET]] {
      override def flatten[F[_ <: EndpointType.Any]](t: K11ET.Id[ET][F]): List[F[EndpointType.Any]] =
        t.asInstanceOf[F[EndpointType.Any]] :: Nil
    }

  @scala.annotation.nowarn
  inline implicit def genProduct[T[_[_ <: EndpointType.Any]]](implicit m: K11ET.ProductGeneric[T]): Flatten[T] = {
    val inst = K11ET.ProductInstances.of[T, Flatten]

    new Flatten[T] {
      override def flatten[F[_ <: EndpointType.Any]](t: T[F]): List[F[EndpointType.Any]] =
        inst.withInstance(t).map { [t[_[_ <: EndpointType.Any]]] => (i: Flatten[t], t: t[F]) => i.flatten(t) }.flatten
    }
  }

}
