package harness.web.client.vdom

import harness.web.client.rawVDOM.VDom.ScopedName

given Conversion[String, TextWidget] = TextWidget(_)
given Conversion[String, ScopedName] = ScopedName(_)
