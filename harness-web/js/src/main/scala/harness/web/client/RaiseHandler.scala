package harness.web.client

import harness.core.*
import harness.web.client.vdom.*
import harness.zio.*
import monocle.Lens
import scala.annotation.nowarn
import zio.*

abstract class RaiseHandler[-A, -S] private (
    runtime: Runtime[HarnessEnv],
) { self =>

  val handleRaise: Raise[A, S] => SHTaskN[Unit]

  // =====| Public API |=====

  final def raiseManyZIO(raises: SHTaskN[List[Raise[A, S]]]*): Unit =
    Unsafe.unsafe {
      runtime.unsafe.run {
        ZIO
          .foreachDiscard(raises) {
            _.flatMap {
              ZIO.foreachDiscard(_)(self.handleRaise)
            }
          }
          .dumpErrorsAndContinueNel(Logger.LogLevel.Error)
      }
    }

  inline final def raiseZIO(raises: SHTaskN[Raise[A, S]]*): Unit = self.raiseManyZIO(raises.map(_.map(_ :: Nil))*)
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
  private[client] final def mapState[S2 <: S, NewS](lens: Lens[S2, NewS]): RaiseHandler[A, NewS] =
    RaiseHandler[A, NewS](runtime) {
      case raise: Raise.ModifyState[NewS] => handleRaise(Raise.ModifyState[S2](lens.modify(raise.modify), raise.reRender))
      case raise: Raise.Action[A]         => handleRaise(raise)
      case raise: Raise.Standard          => handleRaise(raise)
    }

  @nowarn
  private[client] final def mapRaise[NewA, S2 <: S](f: Raise[NewA, S2] => SHTaskN[List[Raise[A, S]]]): RaiseHandler[NewA, S2] =
    RaiseHandler[NewA, S2](runtime) { f(_).flatMap(ZIO.foreachDiscard(_)(handleRaise)) }

  @nowarn
  private[client] final def mapAction[NewA](f: NewA => SHTaskN[List[Raise[A, S]]]): RaiseHandler[NewA, S] =
    RaiseHandler[NewA, S](runtime) {
      case raise: Raise.Action[NewA]   => f(raise.action).flatMap(ZIO.foreachDiscard(_)(handleRaise))
      case raise: Raise.ModifyState[S] => handleRaise(raise)
      case raise: Raise.Standard       => handleRaise(raise)
    }

}
object RaiseHandler {

  private def apply[A, S](runtime: Runtime[HarnessEnv])(_handleRaise: Raise[A, S] => SHTaskN[Unit]): RaiseHandler[A, S] =
    new RaiseHandler[A, S](runtime) {
      override val handleRaise: Raise[A, S] => SHTaskN[Unit] = _handleRaise
    }

  @nowarn
  private[client] def root[A, S](
      renderer: rawVDOM.Renderer,
      stateRef: Ref.Synchronized[S],
      widget: PModifier[A, S, S, Any],
      handleA: A => SHTaskN[List[Raise.StandardOrUpdate[S]]],
      titleF: Either[String, S => String],
      runtime: Runtime[HarnessEnv],
      urlToPage: Url => Page,
  ): RaiseHandler[A, S] =
    new RaiseHandler[A, S](runtime) { self =>
      override val handleRaise: Raise[A, S] => SHTaskN[Unit] = { raise =>
        @nowarn
        def handleStandardOrUpdate(raise: Raise.StandardOrUpdate[S]): SHTaskN[Unit] =
          raise match {
            case raise: Raise.Standard =>
              raise match {
                case history: Raise.History =>
                  history match {
                    case Raise.History.Push(url)    => urlToPage(url).push(renderer, runtime, urlToPage, url)
                    case Raise.History.Replace(url) => urlToPage(url).replace(renderer, runtime, urlToPage, url)
                    case Raise.History.Go(_)        =>
                      // TODO (KR) : This probably needs to have access to the RouteMatcher, or a way to store the page in the history somehow
                      ZIO.failNel(HError.???("History.go"))
                  }
                case Raise.DisplayMessage(message) =>
                  // TODO (KR) : Have a way to display these messages on the page, with styling
                  Logger.log.info(message)
              }
            case raise: Raise.ModifyState[S] =>
              stateRef.updateZIO { state =>
                val newState = raise.modify(state)
                val newVDom = widget.build(self, newState)
                if (raise.reRender) renderer.render(titleF.fold(identity, _(newState)), newVDom).toErrorNel.as(newState)
                else ZIO.succeed(newState)
              }
          }

        raise match {
          case raise: Raise.StandardOrUpdate[S] => handleStandardOrUpdate(raise)
          case raise: Raise.Action[A]           => handleA(raise.action).flatMap(ZIO.foreachDiscard(_)(handleStandardOrUpdate))
        }
      }

    }

}
