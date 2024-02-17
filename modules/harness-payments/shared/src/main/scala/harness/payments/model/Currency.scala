package harness.payments.model

import harness.core.Enum

enum Currency extends Enum[Currency] { case USD }
object Currency extends Enum.Companion[Currency]
