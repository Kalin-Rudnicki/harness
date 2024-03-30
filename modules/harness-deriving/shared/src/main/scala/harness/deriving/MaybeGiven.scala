package harness.deriving

import cats.syntax.option.*

final case class MaybeGiven[+T](toOption: Option[T])
object MaybeGiven {

  inline def apply[T](implicit maybeGiven: MaybeGiven[T]): MaybeGiven[T] = maybeGiven

  inline given inst[T]: MaybeGiven[T] =
    compiletime.summonFrom {
      case t: T => MaybeGiven(t.some)
      case _    => MaybeGiven(None)
    }

}
