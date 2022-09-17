package harness.web.client

import monocle.Lens

sealed trait Raise[+A, +S]
object Raise {

  // =====| Types |=====

  final case class Action[A](action: A) extends Raise[A, Nothing]

  sealed trait StandardOrUpdate[+S] extends Raise[Nothing, S]

  final case class ModifyState[S](modify: S => S, reRender: Boolean) extends Raise.StandardOrUpdate[S]

  sealed trait Standard extends Raise.StandardOrUpdate[Nothing]

  // TODO (KR) : format message
  final case class DisplayMessage(message: String) extends Standard

  sealed trait History extends Standard
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
  // case object ReloadPage extends Standard[Any]

}
