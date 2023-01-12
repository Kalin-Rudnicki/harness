package harness.web.client

import cats.data.NonEmptyList
import harness.core.*
import harness.web.client.vdom.*
import harness.zio.*
import monocle.{Lens, Optional}
import scala.annotation.nowarn
import zio.*

abstract class RaiseHandler[-A, -S] private (
    runtime: Runtime[HarnessEnv],
) { self =>

  val handleRaise: Raise[A, S] => SHTask[Unit]

  // =====| Public API |=====

  final def raiseManyZIO(raises: SHTask[List[Raise[A, S]]]*): Unit =
    PageApp.runZIO(
      runtime,
      ZIO
        .foreachDiscard(raises) { _.flatMap { ZIO.foreachDiscard(_)(self.handleRaise) } }
        .foldZIO(
          { error =>
            ZIO.serviceWithZIO[RunMode] { runMode =>
              ZIO.serviceWithZIO[HError.UserMessage.IfHidden] { ifHidden =>
                ZIO.foreachDiscard {
                  error.toNel.toList.map { err =>
                    Raise.DisplayMessage(PageMessage.error(err.formatMessage(runMode, ifHidden)))
                  }
                }(handleRaise)
              }
            }
          },
          ZIO.succeed(_),
        ),
    )

  inline final def raiseZIO(raises: SHTask[Raise[A, S]]*): Unit = self.raiseManyZIO(raises.map(_.map(_ :: Nil))*)
  inline final def raise(raises: Raise[A, S]*): Unit = self.raiseManyZIO(raises.map { r => ZIO.succeed(r :: Nil) }*)

  // --- State ---

  inline final def updateState[S2 <: S](f: S2 => S2): Unit = raise(Raise.ModifyState(f, true))
  inline final def updateStateNoReRender[S2 <: S](f: S2 => S2): Unit = raise(Raise.ModifyState(f, false))
  inline final def setState(f: => S): Unit = updateState[S] { _ => f }
  inline final def setStateNoReRender(f: => S): Unit = updateStateNoReRender[S] { _ => f }

  // --- History ---

  inline final def pushUrl(url: Url): Unit = raise(Raise.History.Push(url))
  inline final def replaceUrl(url: Url): Unit = raise(Raise.History.Replace(url))

  // --- Transform ---

  @nowarn
  final def mapState[S2 <: S, NewS](lens: Optional[S2, NewS]): RaiseHandler[A, NewS] =
    RaiseHandler[A, NewS](runtime) {
      case raise: Raise.ModifyState[NewS] => handleRaise(Raise.ModifyState[S2](lens.modify(raise.modify), raise.reRender))
      case raise: Raise.Action[A]         => handleRaise(raise)
      case raise: Raise.Standard          => handleRaise(raise)
    }

  @nowarn
  final def mapRaise[NewA, S2 <: S](f: Raise[NewA, S2] => SHTask[List[Raise[A, S]]]): RaiseHandler[NewA, S2] =
    RaiseHandler[NewA, S2](runtime) { f(_).flatMap(ZIO.foreachDiscard(_)(handleRaise)) }

  @nowarn
  final def mapAction[NewA](f: NewA => SHTask[List[Raise[A, S]]]): RaiseHandler[NewA, S] =
    RaiseHandler[NewA, S](runtime) {
      case raise: Raise.Action[NewA]   => f(raise.action).flatMap(ZIO.foreachDiscard(_)(handleRaise))
      case raise: Raise.ModifyState[S] => handleRaise(raise)
      case raise: Raise.Standard       => handleRaise(raise)
    }

}
object RaiseHandler {

  private def apply[A, S](runtime: Runtime[HarnessEnv])(_handleRaise: Raise[A, S] => SHTask[Unit]): RaiseHandler[A, S] =
    new RaiseHandler[A, S](runtime) {
      override val handleRaise: Raise[A, S] => SHTask[Unit] = _handleRaise
    }

  @nowarn
  private[client] def root[A, S](
      renderer: rawVDOM.Renderer,
      stateRef: Ref.Synchronized[PageState[S]],
      widget: PModifier[A, PageState[S], PageState[S], Any],
      handleA: A => SHTask[List[Raise.StandardOrUpdate[PageState[S]]]],
      titleF: Either[String, S => String],
      runtime: Runtime[HarnessEnv],
      urlToPage: Url => Page,
  ): RaiseHandler[A, PageState[S]] =
    new RaiseHandler[A, PageState[S]](runtime) { self =>
      override val handleRaise: Raise[A, PageState[S]] => SHTask[Unit] = { raise =>
        def displayPageMessages(pageMessages: List[PageMessage]): SHTask[Unit] =
          ZIO.foreachDiscard(pageMessages)(pm => Logger.log(pm.logLevel, pm.title)) *>
            stateRef.updateZIO { state =>
              val newState = PageState(state.pageMessages ::: pageMessages, state.state)
              val newVDom = widget.build(self, newState)
              renderer.render(titleF.fold(identity, _(newState.state)), newVDom).as(newState)
            }

        @nowarn
        def handleStandardOrUpdate(raise: Raise.StandardOrUpdate[PageState[S]]): SHTask[Unit] =
          raise match {
            case raise: Raise.Standard =>
              raise match {
                case history: Raise.History =>
                  history match {
                    case Raise.History.Push(url)    => urlToPage(url).push(renderer, runtime, urlToPage, url)
                    case Raise.History.Replace(url) => urlToPage(url).replace(renderer, runtime, urlToPage, url)
                    case Raise.History.Go(_)        =>
                      // TODO (KR) : This probably needs to have access to the RouteMatcher, or a way to store the page in the history somehow
                      ZIO.fail(HError.???("History.go"))
                  }
                case Raise.DisplayMessage(pageMessage) =>
                  displayPageMessages(pageMessage :: Nil)
              }
            case raise: Raise.ModifyState[PageState[S]] =>
              stateRef.updateZIO { state =>
                val newState = raise.modify(state)
                val newVDom = widget.build(self, newState)
                if (raise.reRender) renderer.render(titleF.fold(identity, _(newState.state)), newVDom).as(newState)
                else ZIO.succeed(newState)
              }
          }

        // TODO (KR) : Make sure errors are converted to messages
        raise match {
          case raise: Raise.StandardOrUpdate[PageState[S]] => handleStandardOrUpdate(raise)
          case raise: Raise.Action[A]                      => handleA(raise.action).flatMap(ZIO.foreachDiscard(_)(handleStandardOrUpdate))
        }
      }

    }

}
