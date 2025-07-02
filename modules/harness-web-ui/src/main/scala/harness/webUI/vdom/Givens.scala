package harness.webUI.vdom

import harness.webUI.rawVDOM.VDom.{ClassName, CSSAttr, ScopedName}

given Conversion[String, CModifier] = PModifier.textElement(_)
given Conversion[String, ScopedName] = ScopedName(_)
given Conversion[ClassName, CModifier] = attr => PModifier(attr)
given Conversion[CSSAttr, CModifier] = attr => PModifier.cssAttr(attr.scopedName, attr.value)

given [Action, StateGet, StateSet <: StateGet]
  => Conversion[
    IterableOnce[PModifier[Action, StateGet, StateSet, Any]],
    PModifier[Action, StateGet, StateSet, Unit],
  ] = { mods =>
  PModifier.Simple.unit[Action, StateGet, StateSet] { (rh, state) =>
    mods.iterator.flatMap(_.build(rh, state)).toList
  }
}
