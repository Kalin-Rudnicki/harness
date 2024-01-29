package harness.cli

import scala.annotation.tailrec

sealed trait HelpMessage {

  final def format(config: HelpMessage.Config = HelpMessage.Config.default): String =
    HelpMessage
      .lines(
        " " * config.leftPadding,
        this,
        config,
        if (config.tightenLeft) config.leftWidth.min(HelpMessage.minLeftWidth(this, config)) else config.leftWidth,
      )
      .mkString("\n")

}
object HelpMessage {

  final case class Config(
      tightenLeft: Boolean,
      leftPadding: Int,
      leftWidth: Int,
      centerPadding: Int,
      // TODO (KR) : rightWidth
      indent: Int,
  )
  object Config {

    val default: Config =
      Config(
        tightenLeft = true,
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
      case Text(left, right) => Text(left, right :+ "<optional>")
      case _                 => HelpMessage(Text("Optional:" :: Nil, Nil), Indent(helpMessage))
    }

  def defaultable(helpMessage: HelpMessage, showDefault: Option[String]): HelpMessage = {
    val dflt: String =
      showDefault match {
        case Some(value) => s" ($value)"
        case None        => ""
      }

    helpMessage match {
      case Text(left, right) => Text(left, right :+ s"<defaultable>$dflt")
      case _                 => HelpMessage(Text(s"Defaultable$dflt:" :: Nil, Nil), Indent(helpMessage))
    }
  }

  private final def minLeftWidth(helpMessage: HelpMessage, config: HelpMessage.Config): Int =
    helpMessage match {
      case HelpMessage.Text(leftLines, _) => leftLines.flatMap(_.split("\n").toList).map(_.length).maxOption.getOrElse(0)
      case HelpMessage.Indent(child)      => minLeftWidth(child, config) + config.indent
      case HelpMessage.Joined(children)   => children.map(minLeftWidth(_, config)).maxOption.getOrElse(0)
    }

  // NOTE : At the moment, I believe this will infinite loop if there is a single word that is longer than `remainingLeftWidth`
  @tailrec
  private def alignLeftLine(
      config: HelpMessage.Config,
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
            config,
            tail,
            (if (startOfLine) (" " * leftSpaces) + head else head) :: current,
            stack,
            firstLine,
            false,
            rw,
          )
        else
          alignLeftLine(
            config,
            queue,
            Nil,
            current :: stack,
            false,
            true,
            remainingWidth,
          )
      case Nil =>
        (current :: stack).reverseIterator.map(_.reverse.mkString(" ")).toList
    }

  private final def lines(idt: String, helpMessage: HelpMessage, config: HelpMessage.Config, leftWidth: Int): List[String] =
    helpMessage match {
      case HelpMessage.Text(_leftLines, _rightLines) =>
        val remainingLeftWidth: Int = leftWidth + config.leftPadding - idt.length
        val leftLines = _leftLines.flatMap(_.split("\n").toList)
        val rightLines = _rightLines.flatMap(_.split("\n").toList)

        val alignedLeftLines: List[String] =
          leftLines.flatMap(line => alignLeftLine(config, line.split(" ").toList, Nil, Nil, true, true, remainingLeftWidth))

        alignedLeftLines.zipAll(rightLines, "", "").map { (left, right) =>
          s"$idt$left${" " * (remainingLeftWidth - left.length + config.centerPadding)}$right"
        }
      case HelpMessage.Indent(child)    => lines(idt + (" " * config.indent), child, config, leftWidth)
      case HelpMessage.Joined(children) => children.flatMap(lines(idt, _, config, leftWidth))
    }

}
