package harness.cli

import scala.annotation.tailrec
import scala.collection.mutable

sealed trait HelpMessage {

  final def format(config: HelpMessage.Config = HelpMessage.Config.default): String = {
    def lines(idt: String, helpMessage: HelpMessage): List[String] =
      helpMessage match {
        case HelpMessage.Text(_leftLines, _rightLines) =>
          val remainingLeftWidth: Int = config.leftWidth - idt.length
          val leftLines = _leftLines.flatMap(_.split("\n").toList)
          val rightLines = _rightLines.flatMap(_.split("\n").toList)

          // NOTE : At the moment, I believe this will infinite loop if there is a single word that is longer than `remainingLeftWidth`
          @tailrec
          def alignLeftLine(
              queue: List[String],
              current: List[String],
              stack: List[List[String]],
              firstLine: Boolean,
              startOfLine: Boolean,
              remainingWidth: Int,
          ): List[String] =
            queue match {
              case head :: tail =>
                val leftSpaces: Int =
                  (startOfLine, firstLine) match {
                    case (false, _)    => 1
                    case (true, true)  => 0
                    case (true, false) => config.indent
                  }

                val rw: Int = remainingWidth - head.length - leftSpaces
                if (rw >= 0)
                  alignLeftLine(
                    tail,
                    (if (startOfLine) (" " * leftSpaces) + head else head) :: current,
                    stack,
                    firstLine,
                    false,
                    rw,
                  )
                else
                  alignLeftLine(
                    queue,
                    Nil,
                    current :: stack,
                    false,
                    true,
                    remainingLeftWidth,
                  )
              case Nil =>
                (current :: stack).reverseMap(_.reverse.mkString(" "))
            }

          val alignedLeftLines: List[String] =
            leftLines.flatMap(line => alignLeftLine(line.split(" ").toList, Nil, Nil, true, true, remainingLeftWidth))

          alignedLeftLines.zipAll(rightLines, "", "").map { (left, right) =>
            s"$idt$left${" " * (remainingLeftWidth - left.length + config.centerPadding)}$right"
          }
        case HelpMessage.Indent(child)    => lines(idt + (" " * config.indent), child)
        case HelpMessage.Joined(children) => children.flatMap(lines(idt, _))
      }

    lines(" " * config.leftPadding, this).mkString("\n")
  }

}
object HelpMessage {

  final case class Config(
      leftPadding: Int,
      leftWidth: Int,
      centerPadding: Int,
      // TODO (KR) : rightWidth
      indent: Int,
  )
  object Config {

    val default: Config =
      Config(
        leftPadding = 2,
        leftWidth = 75,
        centerPadding = 4,
        indent = 2,
      )

  }

  def apply(helpMessages: HelpMessage*): HelpMessage =
    helpMessages.toList.flatMap(HelpMessage.toSimpleList) match {
      case head :: Nil => head
      case messages    => HelpMessage.Joined(messages)
    }

  // =====| Types |=====

  sealed trait Simple extends HelpMessage
  final case class Joined(children: List[HelpMessage.Simple]) extends HelpMessage

  final case class Text(leftLines: List[String], rightLines: List[String]) extends HelpMessage.Simple
  final case class Indent(child: HelpMessage) extends HelpMessage.Simple

  // =====| Helpers |=====

  def toSimpleList(helpMessage: HelpMessage): List[HelpMessage.Simple] =
    helpMessage match {
      case simple: Simple   => simple :: Nil
      case Joined(children) => children
    }

  def duplicateParam(name: Name): HelpMessage =
    HelpMessage.Text(s"Unable to build parser due to duplicate param '$name'" :: Nil, Nil)

  def optional(helpMessage: HelpMessage): HelpMessage =
    helpMessage match {
      case Text(left, right) => Text(left, right :+ "(optional)")
      case _                 => HelpMessage(Text("Optional:" :: Nil, Nil), Indent(helpMessage))
    }

  def defaultable(helpMessage: HelpMessage): HelpMessage =
    helpMessage match {
      case Text(left, right) => Text(left, right :+ "(defaultable)")
      case _                 => HelpMessage(Text("Defaultable:" :: Nil, Nil), Indent(helpMessage))
    }

}
