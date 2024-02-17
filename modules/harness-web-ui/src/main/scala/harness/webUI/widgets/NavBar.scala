package harness.webUI.widgets

import harness.webUI.*
import harness.webUI.style.{given, *}
import harness.webUI.vdom.{given, *}

object NavBar {

  def apply[Action, StateGet, StateSet <: StateGet](
      left: PModifier[Action, StateGet, StateSet, Any]*,
  )(
      right: PModifier[Action, StateGet, StateSet, Any]*,
  ): PModifier[Action, StateGet, StateSet, Unit] =
    div(
      DefaultStyleSheet.navBar,
      section(_.wrap, left),
      section(_.expand, Nil),
      section(_.wrap, right),
    )

  def item[Action, StateGet, StateSet <: StateGet](mods: PModifier[Action, StateGet, StateSet, Any]*): PModifier[Action, StateGet, StateSet, Unit] =
    span(
      DefaultStyleSheet.navBar.item,
      mods,
    )

  def linkItem[Action, StateGet, StateSet <: StateGet](url: => Url, mods: PModifier[Action, StateGet, StateSet, Any]*): PModifier[Action, StateGet, StateSet, Unit] =
    span(
      DefaultStyleSheet.navBar.item,
      PModifier.builder.withRaise { rh => onClick := { _ => rh.pushUrl(url) } },
      mods,
    )

  private def section[Action, StateGet, StateSet <: StateGet](
      cssMod: DefaultStyleSheet.navBar.section.type => DefaultStyleSheet.navBar.section.Modifier,
      items: Seq[PModifier[Action, StateGet, StateSet, Any]],
  ): PModifier[Action, StateGet, StateSet, Unit] =
    span(
      DefaultStyleSheet.navBar.section.mod(cssMod),
      items,
    )

}
