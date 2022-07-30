package harness.cli

import cats.syntax.either.*
import cats.syntax.traverse.*
import scala.util.matching.Regex

sealed trait Arg {

  def toArgString: String =
    this match {
      case Arg.ShortParamMulti(name, _)               => s"-$name"
      case Arg.ShortParamSingle(name)                 => s"-$name"
      case Arg.ShortParamSingleWithValue(name, value) => s"-$name=$value"
      case Arg.LongParam(name)                        => s"--$name"
      case Arg.LongParamWithValue(name, value)        => s"--$name=$value"
      case Arg.Value(value)                           => if (value.startsWith("-") && value != "--") s"[-]$value" else value
    }

}
object Arg {
  final case class ShortParamMulti(name: ShortName, subIdx: Int) extends Arg
  final case class ShortParamSingle(name: ShortName) extends Arg
  final case class ShortParamSingleWithValue(name: ShortName, value: String) extends Arg
  final case class LongParam(name: LongName) extends Arg
  final case class LongParamWithValue(name: LongName, value: String) extends Arg
  final case class Value(value: String) extends Arg

  private val EscapedRegex: Regex = "^\\[-](-.*)$".r
  private val ShortParamRegex: Regex = s"^-(${ShortName.regex}+)(?:=(.*))?$$".r
  private val LongParamRegex: Regex = s"^--(${LongName.regex})(?:=(.*))?$$".r

  def parse(arg: String): Either[String, List[Arg]] =
    arg match {
      case EscapedRegex(value) => (Value(value) :: Nil).asRight
      case ShortParamRegex(names, value) =>
        names.toList.traverse(ShortName(_)).flatMap { names =>
          (names, Option(value)) match {
            case (name :: Nil, Some(value)) => (Arg.ShortParamSingleWithValue(name, value) :: Nil).asRight
            case (_, Some(_))               => s"You can only supply a value with a single short-param '$arg'".asLeft
            case (name :: Nil, None)        => (Arg.ShortParamSingle(name) :: Nil).asRight
            case (names, None)              => names.zipWithIndex.map(Arg.ShortParamMulti(_, _)).asRight
          }
        }
      case LongParamRegex(name, value) =>
        LongName(name).map { name =>
          Option(value) match {
            case Some(value) => LongParamWithValue(name, value) :: Nil
            case None        => LongParam(name) :: Nil
          }
        }
      case value => (Value(value) :: Nil).asRight
    }

}
