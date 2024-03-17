package harness.cli

final case class FinalizedParser[+T](
    usedParams: Set[Name],
    helpMessage: HelpMessage,
    helpExtraMessage: HelpMessage,
    parse: IndexedArgs => FinalizedParser.Result[T],
) { self =>

  def apply(args: List[String]): FinalizedParser.Result[T] =
    IndexedArgs.parse(args) match {
      case Right(args) => parse(args)
      case Left(msg)   => FinalizedParser.Result.InvalidArg(msg)
    }

  def apply(args: String*): FinalizedParser.Result[T] =
    self(args.toList)

}
object FinalizedParser {

  sealed trait Result[+T]
  object Result {
    final case class Success[+T](value: T) extends Result[T]

    sealed trait NonSuccess extends Result[Nothing]

    final case class Help(helpExtra: Boolean, message: HelpMessage) extends Result.NonSuccess

    sealed trait FailedToBuildParser extends Result.NonSuccess
    final case class ParamNameConflict(duplicateParam: Name) extends FailedToBuildParser

    final case class ParseFail(fail: ParsingFailure) extends Result.NonSuccess {
      override def toString: String = s"ParseFail:\n$fail"
    }

    final case class InvalidArg(msg: String) extends Result.NonSuccess

  }

}
