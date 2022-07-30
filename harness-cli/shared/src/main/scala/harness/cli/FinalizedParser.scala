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
    final case class Help(helpExtra: Boolean, message: HelpMessage) extends Result[Nothing]
    final case class ParseFail(fail: ParsingFailure) extends Result[Nothing] {
      override def toString: String = s"ParseFail:\n$fail"
    }
    final case class BuildFail(duplicateParam: Name) extends Result[Nothing]
    final case class InvalidArg(msg: String) extends Result[Nothing]
  }

}
