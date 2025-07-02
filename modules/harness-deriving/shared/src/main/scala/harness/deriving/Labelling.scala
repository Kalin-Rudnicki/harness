package harness.deriving

import harness.deriving.Utils.*
import scala.compiletime.*
import scala.deriving.Mirror

final case class Labelling[A](label: String, elemLabels: List[String])
object Labelling {

  inline def apply[A](labelling: Labelling[A]): Labelling[A] = labelling

  inline given of: [A] => (mirror: Mirror.Of[A]) => Labelling[A] =
    Labelling[A](
      constValue[mirror.MirroredLabel & String],
      summonList[mirror.MirroredElemLabels, String],
    )

}
