package harness.console.error

import harness.cli.HelpMessage

final case class ShowHelp(help: HelpMessage) extends Throwable
