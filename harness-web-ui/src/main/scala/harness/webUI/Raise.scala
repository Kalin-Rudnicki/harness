package harness.webUI

import java.util.UUID
import monocle.Lens

sealed trait Raise[+A, +S]
object Raise {

  // =====| Types |=====

  final case class Action[A](action: A) extends Raise[A, Nothing]
  sealed trait StandardOrUpdate[+S] extends Raise[Nothing, S]

  final case class ModifyState[S](modify: S => S, reRender: Boolean) extends Raise.StandardOrUpdate[S]

  inline def updateState[S](f: S => S): ModifyState[S] = ModifyState[S](f, true)
  inline def updateStateNoReRender[S](f: S => S): ModifyState[S] = ModifyState[S](f, false)
  inline def setState[S](f: => S): ModifyState[S] = ModifyState[S]((_: Any) => f, true)
  inline def setStateNoReRender[S](f: => S): ModifyState[S] = ModifyState[S]((_: Any) => f, false)

  sealed trait Standard extends Raise.StandardOrUpdate[Nothing]

  final case class DisplayMessage(message: PageMessage) extends Raise.Standard

  sealed trait History extends Raise.Standard
  object History {
    final case class Push(page: Url) extends History
    final case class Replace(page: Url) extends History
    final case class Go(delta: Int) extends History

    inline def push(url: Url): Push = Push(url)
    inline def replace(url: Url): Replace = Replace(url)
    inline def go(delta: Int): Go = Go(delta)
    val forward: Go = go(1)
    val back: Go = go(-1)
  }

  // TODO (KR) : Have a way to force reload the page
  // case object ReloadPage extends Raise.Standard

}
