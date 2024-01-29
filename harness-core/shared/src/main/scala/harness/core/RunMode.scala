package harness.core

enum RunMode extends Enum[RunMode] { case Prod, Dev }
object RunMode extends Enum.Companion[RunMode]
