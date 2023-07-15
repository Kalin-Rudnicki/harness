package harness.webUI

import cats.syntax.either.*
import harness.http.client.HttpClient
import harness.webUI.vdom.*
import harness.zio.*
import org.scalajs.dom.window
import zio.*

sealed trait Page {

  type A
  type S
  final type PS = PageState[S]

  val fetchState: SHRIO[HttpClient.ClientT, Either[Url, S]]
  val titleF: Either[String, S => String]
  val widget: vdom.PModifier[A, PS, PS, Any]
  val handleA: A => SHRIO[HttpClient.ClientT, List[Raise.StandardOrUpdate[PS]]]

  private final def renderAnd(
      renderer: rawVDOM.Renderer,
      runtime: Runtime[HarnessEnv & HttpClient.ClientT],
      urlToPage: Url => Page,
      url: Url,
  )(actionWithTitle: String => HTask[Unit]): SHRIO[HttpClient.ClientT, Unit] =
    fetchState.trace("Load Page", Logger.LogLevel.Debug, "url" -> url.path.mkString("/", "/", ""), "stage" -> "fetch-state").flatMap {
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

  private[webUI] final def push(renderer: rawVDOM.Renderer, runtime: Runtime[HarnessEnv & HttpClient.ClientT], urlToPage: Url => Page, url: Url): SHRIO[HttpClient.ClientT, Unit] =
    renderAnd(renderer, runtime, urlToPage, url) { title =>
      ZIO.hAttempt(window.history.pushState(null, title, url.toString))
    }

  private[webUI] final def replace(renderer: rawVDOM.Renderer, runtime: Runtime[HarnessEnv & HttpClient.ClientT], urlToPage: Url => Page, url: Url): SHRIO[HttpClient.ClientT, Unit] =
    renderAnd(renderer, runtime, urlToPage, url) { title =>
      ZIO.hAttempt(window.history.replaceState(null, title, url.toString))
    }

  private[webUI] final def replaceNoTrace(renderer: rawVDOM.Renderer, runtime: Runtime[HarnessEnv & HttpClient.ClientT], urlToPage: Url => Page, url: Url): SHRIO[HttpClient.ClientT, Unit] =
    renderAnd(renderer, runtime, urlToPage, url) { _ => ZIO.unit }

  override final def toString: String = s"Page(title = ${titleF.fold(s => s"'$s'", _ => "???")})"

}
object Page {

  def builder: Builder1 = new Builder1

  final class Builder1 private[Page] {
    def fetchStateOrRedirect[State](fetchState: SHRIO[HttpClient.ClientT, Either[Url, State]]): Builder2[State] = Builder2(fetchState)
    def fetchState[State](fetchState: SHRIO[HttpClient.ClientT, State]): Builder2[State] = Builder2(fetchState.asRight)
    def constState[State](constState: => State): Builder2[State] = fetchState(ZIO.succeed(constState))
  }

  final class Builder2[State] private[Page] (
      fetchState: SHRIO[HttpClient.ClientT, Either[Url, State]],
  ) {
    def stateTitle(f: State => String): Builder3[State] = Builder3(fetchState, f.asRight)
    def constTitle(title: String): Builder3[State] = Builder3(fetchState, title.asLeft)
  }

  final class Builder3[State] private[Page] (
      fetchState: SHRIO[HttpClient.ClientT, Either[Url, State]],
      titleF: Either[String, State => String],
  ) {
    inline def body[Action](widget: vdom.PModifier[Action, PageState[State], PageState[State], Any]): Builder4[Action, State] = pagefulBody(widget)
    inline def pagelessBody[Action](widget: vdom.PModifier[Action, State, State, Any]): Builder4[Action, State] = this.body(widget.zoomOut[PageState[State]](_.state))
    def pagefulBody[Action](widget: vdom.PModifier[Action, PageState[State], PageState[State], Any]): Builder4[Action, State] = Builder4(fetchState, titleF, widget)
  }

  final class Builder4[Action, State] private[Page] (
      fetchState: SHRIO[HttpClient.ClientT, Either[Url, State]],
      titleF: Either[String, State => String],
      widget: vdom.PModifier[Action, PageState[State], PageState[State], Any],
  ) { self =>

    def handleA(_handleA: Action => SHRIO[HttpClient.ClientT, List[Raise.StandardOrUpdate[PageState[State]]]]): Page =
      new Page {
        override type A = Action
        override type S = State
        override val fetchState: SHRIO[HttpClient.ClientT, Either[Url, S]] = self.fetchState
        override val titleF: Either[String, S => String] = self.titleF
        override val widget: vdom.PModifier[Action, PS, PS, Any] = self.widget
        override val handleA: Action => SHRIO[HttpClient.ClientT, List[Raise.StandardOrUpdate[PS]]] = _handleA
      }

    def logA: Page =
      handleA { a => Logger.log.warning(s"Ignoring action: $a").as(Nil) }

  }

}