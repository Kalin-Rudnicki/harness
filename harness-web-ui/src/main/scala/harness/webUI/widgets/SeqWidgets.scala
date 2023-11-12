package harness.webUI.widgets

import cats.syntax.traverse.*
import harness.webUI.*
import harness.webUI.vdom.{given, *}
import monocle.*
import zio.Chunk

object SeqWidgets {

  private def listIndexLens[S](idx: Int): Lens[List[S], S] =
    Lens[List[S], S](_(idx))(elem => list => list.updated(idx, elem))

  private def chunkIndexLens[S](idx: Int): Lens[Chunk[S], S] =
    Lens[Chunk[S], S](_(idx))(elem => list => list.updated(idx, elem))

  def listWidget[Action, State, Value](widget: ModifierAV[Action, State, Value]): ModifierAV[Action, List[State], List[Value]] =
    PModifier.Simple[Action, List[State], List[State], List[Value]](
      (rh, listS) =>
        listS.indices.toList.flatMap { idx =>
          widget.imapState(listIndexLens(idx)).build(rh, listS)
        },
      _.traverse(widget.value),
    )

  def chunkWidget[Action, State, Value](widget: ModifierAV[Action, State, Value]): ModifierAV[Action, Chunk[State], Chunk[Value]] =
    PModifier.Simple[Action, Chunk[State], Chunk[State], Chunk[Value]](
      (rh, listS) =>
        listS.indices.toList.flatMap { idx =>
          widget.imapState(chunkIndexLens(idx)).build(rh, listS)
        },
      _.toList.traverse(widget.value).map(Chunk.fromIterable),
    )

}
