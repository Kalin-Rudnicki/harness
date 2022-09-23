package harness.web.client.vdom

import harness.web.client.rawVDOM.VDom.ScopedName

given Conversion[String, CModifier] = PModifier.textElement(_)
given Conversion[String, ScopedName] = ScopedName(_)

given [Action, StateGet, StateSet <: StateGet]: Conversion[
  IterableOnce[PModifier[Action, StateGet, StateSet, Any]],
  PModifier[Action, StateGet, StateSet, Unit],
] = { mods =>
  PModifier.Simple.unit[Action, StateGet, StateSet] { (rh, state) =>
    mods.iterator.flatMap(_.build(rh, state)).toList
  }
}
