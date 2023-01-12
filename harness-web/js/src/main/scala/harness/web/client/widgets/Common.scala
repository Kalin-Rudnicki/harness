package harness.web.client.widgets

import cats.syntax.traverse.*
import harness.web.client.*
import harness.web.client.vdom.{given, *}
import monocle.*

object Common {

  private def lensForIndex[S](idx: Int): Lens[List[S], S] =
    Lens[List[S], S](_(idx))(elem => list => list.updated(idx, elem))

  def listWidget[Action, State, Value](widget: ModifierAV[Action, State, Value]): ModifierAV[Action, List[State], List[Value]] =
    PModifier.Simple[Action, List[State], List[State], List[Value]](
      (rh, listS) =>
        listS.indices.toList.flatMap { idx =>
          widget.imapState(lensForIndex(idx)).build(rh, listS)
        },
      _.traverse(widget.value),
    )

}
