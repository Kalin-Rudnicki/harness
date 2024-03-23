package harness.console.error

sealed trait CommandError extends Throwable {

  override final def getMessage: String = this match {
    case CommandError.BuildingFail(message) => message
    case CommandError.ParsingFail(message) => message
  }

}
object CommandError {

  final case class BuildingFail(message: String) extends CommandError
  final case class ParsingFail(message: String) extends CommandError

}
