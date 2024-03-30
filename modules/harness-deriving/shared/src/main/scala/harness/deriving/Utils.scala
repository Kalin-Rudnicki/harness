package harness.deriving

import scala.compiletime.*

object Utils {

  inline def summonList[T <: Tuple, E]: List[E] =
    inline erasedValue[T] match
      case _: EmptyTuple => Nil
      case _: (a *: b) =>
        constValue[a & E] :: summonList[b, E]

}
