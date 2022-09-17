package harness.web.client.vdom

import harness.web.client.rawVDOM.VDom.ScopedName

given Conversion[String, CModifier] = PModifier.textElement(_)
given Conversion[String, ScopedName] = ScopedName(_)
