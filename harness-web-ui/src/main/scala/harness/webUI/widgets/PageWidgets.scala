package harness.webUI.widgets

import harness.webUI.*
import harness.webUI.vdom.{given, *}
import java.util.UUID
import monocle.macros.GenLens
import zio.*

object PageWidgets {

  val pageMessageWidget: ModifierA[UUID, PageMessage] =
    PModifier.builder.withAction[UUID].withState[PageMessage] { (rh, s) =>
      div(
        CssClass.be("page-messages", "message"),
        s.style,
        s.title,
        onClick := { _ =>
          rh.raise(Raise.Action(s.id))
        },
      )
    }

  val pageMessages: PModifier[Nothing, PageState[Any], PageState[Nothing], Unit] = {
    val tmp: Modifier[PageState[Any]] =
      div(
        CssClass.b("page-messages"),
        Common
          .listWidget(pageMessageWidget)
          .mapActionV[List[PageMessage], List[PageMessage], Nothing] { (_, a) =>
            ZIO.succeed(Raise.updateState[List[PageMessage]](_.filterNot(_.id == a)) :: Nil)
          }
          .zoomOut[PageState[Any]](_.pageMessages),
      )

    tmp.asInstanceOf[PModifier[Nothing, PageState[Any], PageState[Nothing], Unit]]
  }

  def pageBody[Action, State](children: PModifier[Action, State, State, Any]*): ModifierA[Action, PageState[State]] =
    div(
      CssClass.b("page"),
      PageWidgets.pageMessages,
      div(
        CssClass.be("page", "body"),
        children,
      ).zoomOutToPage,
    )

}
