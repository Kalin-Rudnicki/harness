package harness.web.client

import cats.syntax.either.*
import harness.web.client.vdom.*
import harness.zio.*
import org.scalajs.dom.window
import zio.*

sealed trait Page {

  type A
  type S
  final type PS = PageState[S]

  val fetchState: SHTask[Either[Url, S]]
  val titleF: Either[String, S => String]
  val widget: vdom.PModifier[A, PS, PS, Any]
  val handleA: A => SHTask[List[Raise.StandardOrUpdate[PS]]]

  private final def renderAnd(
      renderer: rawVDOM.Renderer,
      runtime: Runtime[HarnessEnv],
      urlToPage: Url => Page,
  )(actionWithTitle: String => HTask[Unit]): SHTask[Unit] =
    fetchState.flatMap {
      case Right(state) =>
        val pageState = PageState.init(state)
        for {
          stateRef <- Ref.Synchronized.make(pageState)
          title = titleF.fold(identity, _(state))
          raiseHandler = RaiseHandler.root[A, S](renderer, stateRef, widget, handleA, titleF, runtime, urlToPage)
          newVDom = widget.build(raiseHandler, pageState)
          _ <- renderer.render(title, newVDom)
          _ <- actionWithTitle(title)
        } yield ()
      case Left(url) =>
        urlToPage(url).replace(renderer, runtime, urlToPage, url)
    }

  private[client] final def push(renderer: rawVDOM.Renderer, runtime: Runtime[HarnessEnv], urlToPage: Url => Page, url: Url): SHTask[Unit] =
    renderAnd(renderer, runtime, urlToPage) { title =>
      ZIO.hAttempt(window.history.pushState(null, title, url.toString))
    }

  private[client] final def replace(renderer: rawVDOM.Renderer, runtime: Runtime[HarnessEnv], urlToPage: Url => Page, url: Url): SHTask[Unit] =
    renderAnd(renderer, runtime, urlToPage) { title =>
      ZIO.hAttempt(window.history.replaceState(null, title, url.toString))
    }

  private[client] final def replaceNoTrace(renderer: rawVDOM.Renderer, runtime: Runtime[HarnessEnv], urlToPage: Url => Page): SHTask[Unit] =
    renderAnd(renderer, runtime, urlToPage) { _ => ZIO.unit }

  override final def toString: String = s"Page(title = ${titleF.fold(s => s"'$s'", _ => "???")})"

}
object Page {

  def builder: Builder1 = new Builder1

  final class Builder1 private[Page] {
    def fetchStateOrRedirect[State](fetchState: SHTask[Either[Url, State]]): Builder2[State] = Builder2(fetchState)
    def fetchState[State](fetchState: SHTask[State]): Builder2[State] = Builder2(fetchState.asRight)
    def constState[State](constState: => State): Builder2[State] = fetchState(ZIO.succeed(constState))
  }

  final class Builder2[State] private[Page] (
      fetchState: SHTask[Either[Url, State]],
  ) {
    def stateTitle(f: State => String): Builder3[State] = Builder3(fetchState, f.asRight)
    def constTitle(title: String): Builder3[State] = Builder3(fetchState, title.asLeft)
  }

  final class Builder3[State] private[Page] (
      fetchState: SHTask[Either[Url, State]],
      titleF: Either[String, State => String],
  ) {
    inline def body[Action](widget: vdom.PModifier[Action, PageState[State], PageState[State], Any]): Builder4[Action, State] = pagefulBody(widget)
    inline def pagelessBody[Action](widget: vdom.PModifier[Action, State, State, Any]): Builder4[Action, State] = this.body(widget.zoomOut[PageState[State]](_.state))
    def pagefulBody[Action](widget: vdom.PModifier[Action, PageState[State], PageState[State], Any]): Builder4[Action, State] = Builder4(fetchState, titleF, widget)
  }

  final class Builder4[Action, State] private[Page] (
      fetchState: SHTask[Either[Url, State]],
      titleF: Either[String, State => String],
      widget: vdom.PModifier[Action, PageState[State], PageState[State], Any],
  ) { self =>

    def handleA(_handleA: Action => SHTask[List[Raise.StandardOrUpdate[PageState[State]]]]): Page =
      new Page {
        override type A = Action
        override type S = State
        override val fetchState: SHTask[Either[Url, S]] = self.fetchState
        override val titleF: Either[String, S => String] = self.titleF
        override val widget: vdom.PModifier[Action, PS, PS, Any] = self.widget
        override val handleA: Action => SHTask[List[Raise.StandardOrUpdate[PS]]] = _handleA
      }

    def logA: Page =
      handleA { a => Logger.log.warning(s"Ignoring action: $a").as(Nil) }

  }

}
