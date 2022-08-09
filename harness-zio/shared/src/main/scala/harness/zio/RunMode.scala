package harness.zio

import harness.core.*
import zio.*

enum RunMode { 
  case Prod, Dev 

  final def formatError(error: HError): String =
    this match {
      case RunMode.Prod => error.userMessage
      case RunMode.Dev => error.fullInternalMessage
    }
  
}
object RunMode {
  val get: URIO[RunMode, RunMode] = ZIO.service[RunMode]
}
