package harness.cli

import cats.data.NonEmptyList

sealed trait ParsingFailure {

  override def toString: String =
    this match {
      case ParsingFailure.And(children) =>
        children.toList.mkString("\n")
      case ParsingFailure.Or(children) =>
        children.toList
          .map {
            _.toString.split("\n").mkString("    ", "\n    ", "")
          }
          .mkString("\nOR\n")
      case ParsingFailure.MissingParam(param) => s"Missing param ${param.formattedName}"
      case ParsingFailure.MalformedValue(param, value, message, helpHint) =>
        s"${param.formattedName} contains malformed value '$value': $message${helpHint.map(str => s"\n    $str").mkString}"
      case ParsingFailure.UnexpectedValue(param, value) => s"${param.formattedName} does not expected a value, but was given: '$value'"
      case ParsingFailure.MissingValue(param)           => s"${param.formattedName} expects a value, but was not given one"
      case ParsingFailure.UnparsedArg(arg)              => s"Remaining unparsed arg '${arg.value.toArgString}'"
    }

}
object ParsingFailure {

  // =====| Types |=====

  sealed trait NonAnd
  sealed trait NonOr

  final case class MissingParam(param: Param) extends ParsingFailure with NonAnd with NonOr
  final case class MalformedValue(param: Param, value: String, message: String, helpHint: List[String]) extends ParsingFailure with NonAnd with NonOr

  final case class UnexpectedValue(param: Param, value: String) extends ParsingFailure with NonAnd with NonOr
  final case class MissingValue(param: Param) extends ParsingFailure with NonAnd with NonOr
  final case class UnparsedArg(arg: Indexed[Arg]) extends ParsingFailure with NonAnd with NonOr

  final case class And(children: NonEmptyList[ParsingFailure with NonAnd]) extends ParsingFailure with NonOr
  final case class Or(children: NonEmptyList[ParsingFailure with NonOr]) extends ParsingFailure with NonAnd

  // =====| Builders |=====

  def and(pf0: ParsingFailure, pfN: ParsingFailure*): ParsingFailure.And = and(NonEmptyList(pf0, pfN.toList))
  def and(pfs: NonEmptyList[ParsingFailure]): ParsingFailure.And = ParsingFailure.And(pfs.flatMap(toNonAndNel))

  def or(pf0: ParsingFailure, pfN: ParsingFailure*): ParsingFailure.Or = or(NonEmptyList(pf0, pfN.toList))
  def or(pfs: NonEmptyList[ParsingFailure]): ParsingFailure.Or = ParsingFailure.Or(pfs.flatMap(toNonOrNel))

  // =====| Helpers |=====

  def toNonAndNel(pf: ParsingFailure): NonEmptyList[ParsingFailure with NonAnd] =
    pf match {
      case And(children)                  => children
      case pf: ParsingFailure with NonAnd => NonEmptyList.one(pf)
    }

  def toNonOrNel(pf: ParsingFailure): NonEmptyList[ParsingFailure with NonOr] =
    pf match {
      case Or(children)                  => children
      case pf: ParsingFailure with NonOr => NonEmptyList.one(pf)
    }

  def containsOnlyMissingParam(fail: ParsingFailure): Boolean =
    fail match {
      case _: MissingParam    => true
      case And(children)      => children.forall(containsOnlyMissingParam)
      case Or(children)       => children.forall(containsOnlyMissingParam)
      case _: MalformedValue  => false
      case _: UnexpectedValue => false
      case _: MissingValue    => false
      case _: UnparsedArg     => false
    }

}
