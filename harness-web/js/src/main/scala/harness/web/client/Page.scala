package harness.web.client

import cats.syntax.either.*
import harness.web.client.vdom.*
import harness.zio.*
import org.scalajs.dom.window
import zio.*

sealed trait Page {

  type A
  type S

  val fetchState: SHTaskN[Either[Url, S]]
  val titleF: Either[String, S => String]
  val widget: vdom.PModifier[A, S, S, Any]
  val handleA: A => SHTaskN[List[Raise.StandardOrUpdate[S]]]

  private final def renderAnd(
      renderer: rawVDOM.Renderer,
      runtime: Runtime[HarnessEnv],
      urlToPage: Url => Page,
  )(actionWithTitle: String => HTask[Unit]): SHTaskN[Unit] =
    fetchState.flatMap {
      case Right(state) =>
        for {
          stateRef <- Ref.Synchronized.make(state)
          title = titleF.fold(identity, _(state))
          raiseHandler = RaiseHandler.root[A, S](renderer, stateRef, widget, handleA, titleF, runtime, urlToPage)
          newVDom = widget.build(raiseHandler, state)
          _ <- renderer.render(title, newVDom).toErrorNel
          _ <- actionWithTitle(title).toErrorNel
        } yield ()
      case Left(url) =>
        urlToPage(url).replace(renderer, runtime, urlToPage, url)
    }

  private[client] final def push(renderer: rawVDOM.Renderer, runtime: Runtime[HarnessEnv], urlToPage: Url => Page, url: Url): SHTaskN[Unit] =
    renderAnd(renderer, runtime, urlToPage) { title =>
      ZIO.hAttempt("Unable to push state to window history")(window.history.pushState(null, title, url.toString))
    }

  private[client] final def replace(renderer: rawVDOM.Renderer, runtime: Runtime[HarnessEnv], urlToPage: Url => Page, url: Url): SHTaskN[Unit] =
    renderAnd(renderer, runtime, urlToPage) { title =>
      ZIO.hAttempt("Unable to replace state in window history")(window.history.replaceState(null, title, url.toString))
    }

  private[client] final def replaceNoTrace(renderer: rawVDOM.Renderer, runtime: Runtime[HarnessEnv], urlToPage: Url => Page): SHTaskN[Unit] =
    renderAnd(renderer, runtime, urlToPage) { _ => ZIO.unit }

  override final def toString: String = s"Page(title = ${titleF.fold(s => s"'$s'", _ => "???")})"

}
object Page {

  def builder: Builder1 = new Builder1

  final class Builder1 private[Page] {
    def fetchStateOrRedirect[State](fetchState: SHTaskN[Either[Url, State]]): Builder2[State] = Builder2(fetchState)
    def fetchState[State](fetchState: SHTaskN[State]): Builder2[State] = Builder2(fetchState.asRight)
    def constState[State](constState: => State): Builder2[State] = fetchState(ZIO.succeed(constState))
  }

  final class Builder2[State] private[Page] (
      fetchState: SHTaskN[Either[Url, State]],
  ) {
    def stateTitle(f: State => String): Builder3[State] = Builder3(fetchState, f.asRight)
    def constTitle(title: String): Builder3[State] = Builder3(fetchState, title.asLeft)
  }

  final class Builder3[State] private[Page] (
      fetchState: SHTaskN[Either[Url, State]],
      titleF: Either[String, State => String],
  ) {
    def body[Action](widget: vdom.PModifier[Action, State, State, Any]): Builder4[Action, State] = Builder4(fetchState, titleF, widget)
  }

  final class Builder4[Action, State] private[Page] (
      fetchState: SHTaskN[Either[Url, State]],
      titleF: Either[String, State => String],
      widget: vdom.PModifier[Action, State, State, Any],
  ) { self =>

    def handleA(_handleA: Action => SHTaskN[List[Raise.StandardOrUpdate[State]]]): Page =
      new Page {
        override type A = Action
        override type S = State
        override val fetchState: SHTaskN[Either[Url, State]] = self.fetchState
        override val titleF: Either[String, State => String] = self.titleF
        override val widget: vdom.PModifier[Action, State, State, Any] = self.widget
        override val handleA: Action => SHTaskN[List[Raise.StandardOrUpdate[State]]] = _handleA
      }

    def logA: Page =
      handleA { a => Logger.log.warning(s"Ignoring action: $a").as(Nil) }

  }

}
