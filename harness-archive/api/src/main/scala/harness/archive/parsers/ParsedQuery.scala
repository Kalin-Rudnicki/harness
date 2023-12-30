package harness.archive.parsers

import cats.data.NonEmptyList
import cats.syntax.either.*
import cats.syntax.option.*
import cats.syntax.traverse.*
import harness.archive.api.db.model as M
import harness.archive.parsers.ParsedQuery.{CompOp, Key}
import slyce.parse.Expression

sealed trait ParsedQuery {

  final def mustPass: List[ParsedQuery.Comparative] =
    this match {
      case ParsedQuery.And(left, right)         => left.mustPass ::: right.mustPass
      case ParsedQuery.Or(_, _)                 => Nil
      case comparative: ParsedQuery.Comparative => comparative :: Nil
    }

  private def getKey(key: ParsedQuery.Key)(app: M.App.Identity, log: M.Log.Identity): Option[String] =
    key match {
      case Key.AppName      => app.name.some
      case Key.AppId        => app.id.toString.some
      case Key.LogLevel     => log.logLevel.map(_.name.toUpperCase)
      case Key.Timestamp    => log.dateTime.toString.some
      case Key.Message      => log.message.some
      case Key.Context(key) => log.context.get(key)
    }
  private def getKey(key: ParsedQuery.Key)(app: M.App.Identity, trace: M.Trace.Identity): Option[String] =
    key match {
      case Key.AppName      => app.name.some
      case Key.AppId        => app.id.toString.some
      case Key.LogLevel     => trace.logLevel.name.toUpperCase.some
      case Key.Timestamp    => trace.startDateTime.toString.some
      case Key.Message      => trace.label.some
      case Key.Context(key) => trace.telemetryContext.get(key).orElse(trace.logContext.get(key))
    }

  private def like(value: String, compValue: String): Boolean =
    value.toUpperCase.contains(compValue.toUpperCase)

  final def toLogFilterFunction: (M.App.Identity, M.Log.Identity) => Boolean =
    this match {
      case ParsedQuery.And(left, right) =>
        val leftF = left.toLogFilterFunction
        val rightF = right.toLogFilterFunction
        (app, log) => leftF(app, log) && rightF(app, log)
      case ParsedQuery.Or(left, right) =>
        val leftF = left.toLogFilterFunction
        val rightF = right.toLogFilterFunction
        (app, log) => leftF(app, log) || rightF(app, log)
      case ParsedQuery.EqualMany(key, values) =>
        val compValues = values.toList.toSet
        getKey(key)(_, _).fold(false)(compValues.contains)
      case ParsedQuery.Exists(key) =>
        getKey(key)(_, _).nonEmpty
      case ParsedQuery.LikeMany(key, values) =>
        val compValues = values.toList
        getKey(key)(_, _).fold(false)(value => compValues.exists(like(value, _)))
      case ParsedQuery.Comp(key, op, compValue) =>
        // TODO (KR) : `!=` None -> true?
        // TODO (KR) : better comparison
        getKey(key)(_, _).fold(false) { value =>
          op match {
            case ParsedQuery.CompOp.Eq        => value == compValue
            case ParsedQuery.CompOp.NotEq     => value != compValue
            case ParsedQuery.CompOp.Like      => like(value, compValue)
            case ParsedQuery.CompOp.Greater   => value > compValue
            case ParsedQuery.CompOp.Less      => value < compValue
            case ParsedQuery.CompOp.GreaterEq => value >= compValue
            case ParsedQuery.CompOp.LessEq    => value <= compValue
          }
        }
    }

  final def toTraceFilterFunction: (M.App.Identity, M.Trace.Identity) => Boolean =
    this match {
      case ParsedQuery.And(left, right) =>
        val leftF = left.toTraceFilterFunction
        val rightF = right.toTraceFilterFunction
        (app, trace) => leftF(app, trace) && rightF(app, trace)
      case ParsedQuery.Or(left, right) =>
        val leftF = left.toTraceFilterFunction
        val rightF = right.toTraceFilterFunction
        (app, trace) => leftF(app, trace) || rightF(app, trace)
      case ParsedQuery.EqualMany(key, values) =>
        val compValues = values.toList.toSet
        getKey(key)(_, _).fold(false)(compValues.contains)
      case ParsedQuery.Exists(key) =>
        getKey(key)(_, _).nonEmpty
      case ParsedQuery.LikeMany(key, values) =>
        val compValues = values.toList
        getKey(key)(_, _).fold(false)(value => compValues.exists(like(value, _)))
      case ParsedQuery.Comp(key, op, compValue) =>
        // TODO (KR) : `!=` None -> true?
        // TODO (KR) : better comparison
        getKey(key)(_, _).fold(false) { value =>
          op match {
            case ParsedQuery.CompOp.Eq        => value == compValue
            case ParsedQuery.CompOp.NotEq     => value != compValue
            case ParsedQuery.CompOp.Like      => like(value, compValue)
            case ParsedQuery.CompOp.Greater   => value > compValue
            case ParsedQuery.CompOp.Less      => value < compValue
            case ParsedQuery.CompOp.GreaterEq => value >= compValue
            case ParsedQuery.CompOp.LessEq    => value <= compValue
          }
        }
    }

}
object ParsedQuery {

  sealed trait Logical extends ParsedQuery
  sealed trait Comparative extends ParsedQuery

  final case class And(left: ParsedQuery, right: ParsedQuery) extends ParsedQuery.Logical
  final case class Or(left: ParsedQuery, right: ParsedQuery) extends ParsedQuery.Logical

  final case class EqualMany(key: Key, values: NonEmptyList[String]) extends ParsedQuery.Comparative
  final case class LikeMany(key: Key, values: NonEmptyList[String]) extends ParsedQuery.Comparative
  final case class Exists(key: Key) extends ParsedQuery.Comparative
  final case class Comp(key: Key, op: CompOp, value: String) extends ParsedQuery.Comparative

  // =====|  |=====

  def from(expr: Expression[QueryParser.NonTerminal.Query.Operand, QueryParser.NonTerminal.Query.Operator]): Either[String, ParsedQuery] =
    expr match {
      case Expression.Leaf(part) =>
        part match {
          case QueryParser.NonTerminal.Query3._1(text(key), _, _, texts(values), _) => Key.from(key).map(ParsedQuery.EqualMany(_, values))
          case QueryParser.NonTerminal.Query3._2(text(key), _, _, texts(values), _) => Key.from(key).map(ParsedQuery.LikeMany(_, values))
          case QueryParser.NonTerminal.Query3._3(text(key), _)                      => Key.from(key).map(ParsedQuery.Exists.apply)
          case QueryParser.NonTerminal.Query3._4(text(key), op, text(value))        => Key.from(key).map(ParsedQuery.Comp(_, CompOp.from(op), value))
          case QueryParser.NonTerminal.Query3._5(_, part, _)                        => ParsedQuery.from(part.toExpr)
          case _                                                                    => throw new RuntimeException(s"invalid part: $part")
        }
      case Expression.Node(left, QueryParser.Terminal.`&`(_), right) =>
        for {
          left <- ParsedQuery.from(left)
          right <- ParsedQuery.from(right)
        } yield ParsedQuery.And(left, right)
      case Expression.Node(left, QueryParser.Terminal.`|`(_), right) =>
        for {
          left <- ParsedQuery.from(left)
          right <- ParsedQuery.from(right)
        } yield ParsedQuery.Or(left, right)
    }

  sealed trait Key
  object Key {

    case object AppName extends Key
    case object AppId extends Key
    case object LogLevel extends Key
    case object Timestamp extends Key
    case object Message extends Key
    final case class Context(key: String) extends Key

    private val contextRegex = "^context\\.([A-Za-z0-9_\\-.]+)$".r
    def from(string: String): Either[String, Key] =
      string match {
        case "app-name"        => Key.AppName.asRight
        case "app-id"          => Key.AppId.asRight
        case "log-level"       => Key.LogLevel.asRight
        case "timestamp"       => Key.Timestamp.asRight
        case "message"         => Key.Message.asRight
        case contextRegex(key) => Key.Context(key).asRight
        case _                 => s"Invalid key '$string'".asLeft
      }

  }

  sealed trait CompOp
  object CompOp {
    case object Eq extends CompOp
    case object NotEq extends CompOp
    case object Like extends CompOp
    case object Greater extends CompOp
    case object Less extends CompOp
    case object GreaterEq extends CompOp
    case object LessEq extends CompOp

    def from(op: QueryParser.NonTerminal.CompOp): CompOp =
      op.lift match {
        case QueryParser.Terminal.`=`(_)  => CompOp.Eq
        case QueryParser.Terminal.`!=`(_) => CompOp.NotEq
        case QueryParser.Terminal.`~=`(_) => CompOp.Like
        case QueryParser.Terminal.`<`(_)  => CompOp.Less
        case QueryParser.Terminal.`>`(_)  => CompOp.Greater
        case QueryParser.Terminal.`<=`(_) => CompOp.LessEq
        case QueryParser.Terminal.`>=`(_) => CompOp.GreaterEq
      }
  }

  // =====|  |=====

  private object text {
    def unapply(text: QueryParser.NonTerminal.Text): Option[String] =
      (text match {
        case QueryParser.NonTerminal.Text._1(_, charsList, _) =>
          charsList.toList.map {
            case QueryParser.NonTerminal.TextElem._1(chars)                                    => chars.text
            case QueryParser.NonTerminal.TextElem._2(QueryParser.Terminal.escChar("n", _))     => "\n"
            case QueryParser.NonTerminal.TextElem._2(QueryParser.Terminal.escChar("t", _))     => "\t"
            case QueryParser.NonTerminal.TextElem._2(QueryParser.Terminal.escChar("\\", _))    => "\\"
            case QueryParser.NonTerminal.TextElem._2(QueryParser.Terminal.escChar(escChar, _)) => escChar
          }.mkString
        case QueryParser.NonTerminal.Text._2(chars) => chars.text
      }).some
  }

  private object texts {
    def unapply(texts: QueryParser.NonTerminal.AnonList1Head): Option[NonEmptyList[String]] =
      texts.toNonEmptyList.traverse(text.unapply)
  }

}
