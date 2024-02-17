package harness.csv

import cats.syntax.either.*
import cats.syntax.option.*
import harness.core.*
import scala.annotation.tailrec

object Parser {

  private sealed trait State
  private object State {

    case object Start extends State
    final case class NotQuoted(rChars: List[Char]) extends State
    final case class Quoted(rChars: List[Char], lastCharIsQuote: Boolean) extends State

  }

  def parse(csv: String): Either[String, List[IArray[Option[String]]]] = {
    @tailrec
    def loop(
        queue: List[Char],
        state: State,
        rPrevLines: List[IArray[Option[String]]],
        rCurrentLine: List[Option[String]],
        lineNo: Int,
    ): Either[String, List[IArray[Option[String]]]] =
      queue match {
        case Nil =>
          state match {
            case State.Start =>
              if (rCurrentLine.nonEmpty) (IArray.from((None :: rCurrentLine).reverse) :: rPrevLines).reverse.asRight
              else rPrevLines.reverse.asRight
            case State.NotQuoted(rChars)    => (IArray.from((rChars.reverse.mkString.some :: rCurrentLine).reverse) :: rPrevLines).reverse.asRight
            case State.Quoted(rChars, true) => (IArray.from((rChars.reverse.mkString.some :: rCurrentLine).reverse) :: rPrevLines).reverse.asRight
            case State.Quoted(_, false)     => s"Unexpected EOF on line $lineNo (expected closing quote)".asLeft
          }
        case '\n' :: tail =>
          state match {
            case State.Start                 => loop(tail, State.Start, IArray.from((None :: rCurrentLine).reverse) :: rPrevLines, Nil, lineNo + 1)
            case State.NotQuoted(rChars)     => loop(tail, State.Start, IArray.from((rChars.reverse.mkString.some :: rCurrentLine).reverse) :: rPrevLines, Nil, lineNo + 1)
            case State.Quoted(rChars, true)  => loop(tail, State.Start, IArray.from((rChars.reverse.mkString.some :: rCurrentLine).reverse) :: rPrevLines, Nil, lineNo + 1)
            case State.Quoted(rChars, false) => loop(tail, State.Quoted('\n' :: rChars, false), rPrevLines, rCurrentLine, lineNo + 1)
          }
        case '"' :: tail =>
          state match {
            case State.Start                 => loop(tail, State.Quoted(Nil, false), rPrevLines, rCurrentLine, lineNo)
            case State.NotQuoted(_)          => s"Unexpected quote on line $lineNo (can only use quote if cell is quoted)".asLeft
            case State.Quoted(rChars, true)  => loop(tail, State.Quoted('"' :: rChars, false), rPrevLines, rCurrentLine, lineNo)
            case State.Quoted(rChars, false) => loop(tail, State.Quoted(rChars, true), rPrevLines, rCurrentLine, lineNo)
          }
        case ',' :: tail =>
          state match {
            case State.Start                 => loop(tail, State.Start, rPrevLines, None :: rCurrentLine, lineNo)
            case State.NotQuoted(rChars)     => loop(tail, State.Start, rPrevLines, rChars.reverse.mkString.some :: rCurrentLine, lineNo)
            case State.Quoted(rChars, true)  => loop(tail, State.Start, rPrevLines, rChars.reverse.mkString.some :: rCurrentLine, lineNo)
            case State.Quoted(rChars, false) => loop(tail, State.Quoted(',' :: rChars, false), rPrevLines, rCurrentLine, lineNo)
          }
        case head :: tail =>
          state match {
            case State.Start                 => loop(tail, State.NotQuoted(head :: Nil), rPrevLines, rCurrentLine, lineNo)
            case State.NotQuoted(rChars)     => loop(tail, State.NotQuoted(head :: rChars), rPrevLines, rCurrentLine, lineNo)
            case State.Quoted(_, true)       => s"Unexpected char ${head.unesc} on line $lineNo (expected '\"' or '\\n')".asLeft
            case State.Quoted(rChars, false) => loop(tail, State.Quoted(head :: rChars, false), rPrevLines, rCurrentLine, lineNo)
          }
      }

    loop(csv.toList, State.Start, Nil, Nil, 1)
  }

}
