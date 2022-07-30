package harness.cli

import cats.syntax.option.*
import scala.annotation.tailrec

opaque type FindFunction[+A] = IndexedArgs => FindFunction.Result[A]
extension [A](self: FindFunction[A]) {

  def attemptToFind(args: IndexedArgs): FindFunction.Result[A] = self(args)

  def map[A2](f: A => A2): FindFunction[A2] = self(_).map(f)

  def as[A2](f: => A2): FindFunction[A2] = self.map(_ => f)

  def ||[A2 >: A](other: FindFunction[A2]): FindFunction[A2] =
    args => self(args) || other(args)

  def toParseFunction(primaryParam: Param): IndexedArgs => Parser.ParseResult[A] = { args =>
    self(args) match {
      case FindFunction.Result.Found(before, arg, after) => Parser.ParseResult.Success(before ::: after, arg)
      case FindFunction.Result.NotFound                  => Parser.ParseResult.Fail(args, ParsingFailure.MissingParam(primaryParam))
      case FindFunction.Result.Fail(fail, remaining)     => Parser.ParseResult.Fail(remaining, fail(primaryParam))
    }
  }

}
extension [A](self: FindFunction[FindFunction.TmpResult[A]]) {

  def noValues: FindFunction[A] =
    self(_) match {
      case FindFunction.Result.Found(before, arg, after) =>
        arg match {
          case FindFunction.TmpResult.WithoutValue(arg, _) => FindFunction.Result.Found(before, arg, after)
          case FindFunction.TmpResult.WithValue(_, value)  => FindFunction.Result.Fail(ParsingFailure.UnexpectedValue(_, value), before ::: after)
        }
      case FindFunction.Result.NotFound   => FindFunction.Result.NotFound
      case fail: FindFunction.Result.Fail => fail
    }

  def constValue[V](v: => V): FindFunction[V] =
    self.noValues(_).map { _ => v }

  def singleValueWithName: FindFunction[(A, String)] =
    self(_) match {
      case FindFunction.Result.Found(before, arg, after) =>
        arg match {
          case FindFunction.TmpResult.WithValue(arg, value) => FindFunction.Result.Found(before, (arg, value), after)
          case FindFunction.TmpResult.WithoutValue(arg, true) =>
            after match {
              case Indexed(value: Arg.Value, _) :: tail => FindFunction.Result.Found(before, (arg, value.value), tail)
              case _                                    => FindFunction.Result.Fail(ParsingFailure.MissingValue(_), before ::: after)
            }
          case FindFunction.TmpResult.WithoutValue(_, false) => FindFunction.Result.Fail(ParsingFailure.MissingValue(_), before ::: after)
        }
      case FindFunction.Result.NotFound   => FindFunction.Result.NotFound
      case fail: FindFunction.Result.Fail => fail
    }

  def singleValue: FindFunction[String] =
    self.singleValueWithName(_).map(_._2)

}
object FindFunction {

  sealed trait Result[+A] {

    final def map[A2](f: A => A2): Result[A2] =
      this match {
        case Result.Found(before, arg, after) => Result.Found(before, f(arg), after)
        case Result.NotFound                  => Result.NotFound
        case fail: Result.Fail                => fail
      }

    final def flatMap[A2](f: (IndexedArgs, A, IndexedArgs) => Result[A2]): Result[A2] =
      this match {
        case Result.Found(before, arg, after) => f(before, arg, after)
        case Result.NotFound                  => Result.NotFound
        case fail: Result.Fail                => fail
      }

    final def ||[A2 >: A](or: => Result[A2]): Result[A2] =
      this match {
        case found: Result.Found[A] => found
        case Result.NotFound        => or
        case fail: Result.Fail      => fail
      }

  }
  object Result {
    case object NotFound extends Result[Nothing]
    final case class Found[+A](before: IndexedArgs, arg: A, after: IndexedArgs) extends Result[A]
    final case class Fail(fail: Param => ParsingFailure, remaining: IndexedArgs) extends Result[Nothing]
  }

  // =====| Builders |=====

  sealed trait TmpResult[+A]
  object TmpResult {
    final case class WithoutValue[+A](arg: A, canLookForValue: Boolean) extends TmpResult[A]
    final case class WithValue[+A](arg: A, value: String) extends TmpResult[A]
  }

  def forParam(param: Param.LongWithValue): FindFunction[String] =
    find(param.name).singleValue

  def forParam(param: Param.ShortWithValue): FindFunction[String] =
    find(param.name).singleValue

  def forParam(param: Param.LongToggle): FindFunction[Boolean] =
    find(param.trueName).constValue(true) || find(param.falseName).constValue(false)

  def forParam(param: Param.ShortToggle): FindFunction[Boolean] =
    find(param.trueName).constValue(true) || find(param.falseName).constValue(false)

  def forParam(param: Param.LongFlag): FindFunction[Unit] =
    find(param.name).constValue { () }

  def forParam(param: Param.ShortFlag): FindFunction[Unit] =
    find(param.name).constValue { () }

  def forValue: FindFunction[String] =
    findGeneric { case value: Arg.Value =>
      value.toArgString
    }

  // =====| Helpers |=====

  def findGeneric[A](f: PartialFunction[Arg, A]): FindFunction[A] = {
    val liftedF: Arg => Option[A] = f.lift
    val iArgF: Indexed[Arg] => Option[A] = iArg => liftedF(iArg.value)

    @tailrec
    def loop(
        queue: IndexedArgs,
        stack: IndexedArgs,
    ): Result[A] =
      queue match {
        case head :: tail =>
          iArgF(head) match {
            case Some(a) => Result.Found(stack.reverse, a, tail)
            case None    => loop(tail, head :: stack)
          }
        case Nil => Result.NotFound
      }

    loop(_, Nil)
  }

  private def find(name: LongName): FindFunction[TmpResult[LongName]] =
    findGeneric {
      case a: Arg.LongParam if a.name == name          => TmpResult.WithoutValue(a.name, true)
      case a: Arg.LongParamWithValue if a.name == name => TmpResult.WithValue(a.name, a.value)
    }

  private def find(name: ShortName): FindFunction[TmpResult[ShortName]] =
    findGeneric {
      case a: Arg.ShortParamSingle if a.name == name          => TmpResult.WithoutValue(a.name, true)
      case a: Arg.ShortParamSingleWithValue if a.name == name => TmpResult.WithValue(a.name, a.value)
      case a: Arg.ShortParamMulti if a.name == name           => TmpResult.WithoutValue(a.name, false)
    }

}
