package harness.webUI.widgets

import harness.webUI.*
import harness.webUI.vdom.{given, *}

object NavBar {

  def apply[Action, StateGet, StateSet <: StateGet](
      left: PModifier[Action, StateGet, StateSet, Any]*,
  )(
      right: PModifier[Action, StateGet, StateSet, Any]*,
  ): PModifier[Action, StateGet, StateSet, Unit] =
    div(
      CssClass.b("nav-bar"),
      section("wrap", left),
      section("expand", Nil),
      section("wrap", right),
    )

  def item[Action, StateGet, StateSet <: StateGet](mods: PModifier[Action, StateGet, StateSet, Any]*): PModifier[Action, StateGet, StateSet, Unit] =
    span(
      CssClass.be("nav-bar", "item"),
      mods,
    )

  def linkItem[Action, StateGet, StateSet <: StateGet](url: => Url, mods: PModifier[Action, StateGet, StateSet, Any]*): PModifier[Action, StateGet, StateSet, Unit] =
    span(
      CssClass.be("nav-bar", "item"),
      PModifier.builder.withRaise { rh => onClick := { _ => rh.pushUrl(url) } },
      mods,
    )

  private def section[Action, StateGet, StateSet <: StateGet](cssMod: String, items: Seq[PModifier[Action, StateGet, StateSet, Any]]): PModifier[Action, StateGet, StateSet, Unit] =
    span(
      CssClass.be("nav-bar", "section", cssMod),
      items,
    )

}
