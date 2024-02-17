package harness.cli

import cats.syntax.option.*

object Defaultable {

  sealed trait Optional[+V] {

    final def map[V2](f: V => V2): Optional[V2] =
      this match {
        case Defaultable.Some(value) => Defaultable.Some(f(value))
        case Defaultable.Auto        => Defaultable.Auto
        case Defaultable.None        => Defaultable.None
      }

  }
  sealed trait NonOptional[+V] extends Optional[V]

  case object Auto extends NonOptional[Nothing]
  final case class Some[+V](value: V) extends NonOptional[V]
  case object None extends Optional[Nothing]
}
