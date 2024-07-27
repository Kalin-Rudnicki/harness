package harness.core

import cats.syntax.option.*

extension [K, V](self: Map[K, V]) {

  def +|+(that: Map[K, V]): Map[K, MapOps.Presence[V]] =
    (self.keySet | that.keySet).toList.flatMap { key =>
      (self.get(key), that.get(key)) match {
        case (Some(fromSelf), Some(fromThat)) if fromSelf == fromThat => (key, MapOps.Presence.BothSame(fromSelf)).some
        case (Some(fromSelf), Some(fromThat))                         => (key, MapOps.Presence.BothDifferent(fromSelf, fromThat)).some
        case (Some(fromSelf), None)                                   => (key, MapOps.Presence.Left(fromSelf)).some
        case (None, Some(fromThat))                                   => (key, MapOps.Presence.Right(fromThat)).some
        case (None, None)                                             => None
      }
    }.toMap

}

object MapOps {

  sealed trait Presence[A]
  object Presence {
    final case class Left[A](value: A) extends Presence[A]
    final case class Right[A](value: A) extends Presence[A]
    final case class BothSame[A](value: A) extends Presence[A]
    final case class BothDifferent[A](leftValue: A, rightValue: A) extends Presence[A]
  }

}
