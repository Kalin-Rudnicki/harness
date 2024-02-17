package harness.webUI

import cats.syntax.either.*
import harness.http.client.HttpClient
import harness.webUI.error.UIError
import harness.webUI.vdom.*
import harness.zio.*
import monocle.macros.GenLens
import org.scalajs.dom.window
import scala.annotation.nowarn
import zio.*

sealed trait Page {

  type A
  type S
  final type PS = PageState[S]

  val fetchState: PageLoadTask[S]
  val postLoad: (S, RaiseHandler[Nothing, PageState[S]]) => PageTask[Unit]
  val titleF: Either[String, S => String]
  val widget: vdom.PModifier[A, PS, PS, Any]
  val handleA: A => PageTask[List[Raise.StandardOrUpdate[PS]]]

  private final def renderAnd(
      renderer: rawVDOM.Renderer,
      runtime: Runtime[HarnessEnv & HttpClient.ClientT],
      urlToPage: Url => Page,
      url: Url,
  )(actionWithTitle: String => Task[Unit]): PageTask[Unit] =
    fetchState
      .telemetrize("Load Page", Logger.LogLevel.Debug, "url" -> url.path.mkString("/", "/", ""), "stage" -> "fetch-state")
      .foldZIO(
        {
          case UIError.Redirect(url) => urlToPage(url).replace(renderer, runtime, urlToPage, url)
          case fail: UIError.Failure => ZIO.fail(fail)
        },
        { state =>
          val pageState = PageState.init(state)
          for {
            stateRef <- Ref.Synchronized.make(pageState)
            title = titleF.fold(identity, _(state))
            raiseHandler = RaiseHandler.root[A, S](renderer, stateRef, widget, handleA, titleF, runtime, urlToPage)
            newVDom = widget.build(raiseHandler, pageState)
            _ <- renderer.render(title, newVDom).mapError(UIError.Failure.internalDefect(_))
            _ <- actionWithTitle(title).mapError(UIError.Failure.internalDefect(_))
            _ <- raiseHandler.executeWith(postLoad(state, raiseHandler))
          } yield ()
        },
      )

  private[webUI] final def push(renderer: rawVDOM.Renderer, runtime: Runtime[HarnessEnv & HttpClient.ClientT], urlToPage: Url => Page, url: Url): PageTask[Unit] =
    renderAnd(renderer, runtime, urlToPage, url) { title =>
      ZIO.attempt(window.history.pushState(null, title, url.toString))
    }

  private[webUI] final def replace(renderer: rawVDOM.Renderer, runtime: Runtime[HarnessEnv & HttpClient.ClientT], urlToPage: Url => Page, url: Url): PageTask[Unit] =
    renderAnd(renderer, runtime, urlToPage, url) { title =>
      ZIO.attempt(window.history.replaceState(null, title, url.toString))
    }

  private[webUI] final def replaceNoTrace(renderer: rawVDOM.Renderer, runtime: Runtime[HarnessEnv & HttpClient.ClientT], urlToPage: Url => Page, url: Url): PageTask[Unit] =
    renderAnd(renderer, runtime, urlToPage, url) { _ => ZIO.unit }

  override final def toString: String = s"Page(title = ${titleF.fold(s => s"'$s'", _ => "_ => ???")})"

}
object Page {

  def builder: Builder1 = new Builder1

  final class Builder1 private[Page] {
    def fetchState[State](fetchState: PageLoadTask[State]): Builder2[State] = Builder2(fetchState)
    def constState[State](constState: => State): Builder2[State] = fetchState(ZIO.succeed(constState))
  }

  final class Builder2[State] private[Page] (
      fetchState: PageLoadTask[State],
  ) {
    def postLoad(f: State => PageTask[Unit]): Builder3[State] = postLoadRH((s, _) => f(s))
    @nowarn // compiler being stupid
    def postLoadRHS(f: (State, RaiseHandler[Nothing, State]) => PageTask[Unit]): Builder3[State] = postLoadRH((s, rh) => f(s, rh.mapState(GenLens[PageState[State]](_.state))))
    def postLoadRH(f: (State, RaiseHandler[Nothing, PageState[State]]) => PageTask[Unit]): Builder3[State] = Builder3(fetchState, f)
    def stateTitle(f: State => String): Builder4[State] = Builder4(fetchState, (_, _) => ZIO.unit, f.asRight)
    def constTitle(title: String): Builder4[State] = Builder4(fetchState, (_, _) => ZIO.unit, title.asLeft)
  }

  final class Builder3[State] private[Page] (
      fetchState: PageLoadTask[State],
      postLoad: (State, RaiseHandler[Nothing, PageState[State]]) => PageTask[Unit],
  ) {
    def stateTitle(f: State => String): Builder4[State] = Builder4(fetchState, postLoad, f.asRight)
    def constTitle(title: String): Builder4[State] = Builder4(fetchState, postLoad, title.asLeft)
  }

  final class Builder4[State] private[Page] (
      fetchState: PageLoadTask[State],
      postLoad: (State, RaiseHandler[Nothing, PageState[State]]) => PageTask[Unit],
      titleF: Either[String, State => String],
  ) {
    inline def body[Action](widget: vdom.PModifier[Action, PageState[State], PageState[State], Any]): Builder5[Action, State] = pagefulBody(widget)
    inline def pagelessBody[Action](widget: vdom.PModifier[Action, State, State, Any]): Builder5[Action, State] = this.body(widget.zoomOut[PageState[State]](_.state))
    def pagefulBody[Action](widget: vdom.PModifier[Action, PageState[State], PageState[State], Any]): Builder5[Action, State] = Builder5(fetchState, postLoad, titleF, widget)
  }

  final class Builder5[Action, State] private[Page] (
      fetchState: PageLoadTask[State],
      postLoad: (State, RaiseHandler[Nothing, PageState[State]]) => PageTask[Unit],
      titleF: Either[String, State => String],
      widget: vdom.PModifier[Action, PageState[State], PageState[State], Any],
  ) { self =>

    def handleA(_handleA: Action => PageTask[List[Raise.StandardOrUpdate[PageState[State]]]]): Page =
      new Page {
        override type A = Action
        override type S = State
        override val fetchState: PageLoadTask[S] = self.fetchState
        override val postLoad: (State, RaiseHandler[Nothing, PageState[State]]) => PageTask[Unit] = self.postLoad
        override val titleF: Either[String, S => String] = self.titleF
        override val widget: vdom.PModifier[Action, PS, PS, Any] = self.widget
        override val handleA: Action => PageTask[List[Raise.StandardOrUpdate[PS]]] = _handleA
      }

    def logA: Page =
      handleA { a => Logger.log.warning(s"Ignoring action: $a").as(Nil) }

    def noAction(implicit ev: Action <:< Nothing): Page =
      handleA { _ => ZIO.dieMessage("Should not be possible to have an action") }

  }

}
