package harness.core

import cats.data.*
import cats.syntax.list.*
import cats.syntax.option.*
import scala.annotation.tailrec

sealed trait ColorString {

  def copy(cpF: ColorString.Color => ColorString.Color): ColorString

  def overwrite(color: ColorString.Color): ColorString =
    this.copy(_.overwrite(color))

  def underwrite(color: ColorString.Color): ColorString =
    this.copy(_.underwrite(color))

  // =====| Foreground |=====

  def black: ColorString =
    this.copy(_.copy(fg = Color.Named.Black.some))

  def red: ColorString =
    this.copy(_.copy(fg = Color.Named.Red.some))

  def green: ColorString =
    this.copy(_.copy(fg = Color.Named.Green.some))

  def yellow: ColorString =
    this.copy(_.copy(fg = Color.Named.Yellow.some))

  def blue: ColorString =
    this.copy(_.copy(fg = Color.Named.Blue.some))

  def magenta: ColorString =
    this.copy(_.copy(fg = Color.Named.Magenta.some))

  def cyan: ColorString =
    this.copy(_.copy(fg = Color.Named.Cyan.some))

  def white: ColorString =
    this.copy(_.copy(fg = Color.Named.White.some))

  def rgb(r: Int, g: Int, b: Int): ColorString =
    this.copy(_.copy(fg = Color.RGB(r, g, b).some))

  def dflt: ColorString =
    this.copy(_.copy(fg = Color.Default.some))

  def noFg: ColorString =
    this.copy(_.copy(fg = None))

  // =====| Background |=====

  def blackBg: ColorString =
    this.copy(_.copy(bg = Color.Named.Black.some))

  def redBg: ColorString =
    this.copy(_.copy(bg = Color.Named.Red.some))

  def greenBg: ColorString =
    this.copy(_.copy(bg = Color.Named.Green.some))

  def yellowBg: ColorString =
    this.copy(_.copy(bg = Color.Named.Yellow.some))

  def blueBg: ColorString =
    this.copy(_.copy(bg = Color.Named.Blue.some))

  def magentaBg: ColorString =
    this.copy(_.copy(bg = Color.Named.Magenta.some))

  def cyanBg: ColorString =
    this.copy(_.copy(bg = Color.Named.Cyan.some))

  def whiteBg: ColorString =
    this.copy(_.copy(bg = Color.Named.White.some))

  def rgbBg(r: Int, g: Int, b: Int): ColorString =
    this.copy(_.copy(bg = Color.RGB(r, g, b).some))

  def dfltBg: ColorString =
    this.copy(_.copy(bg = Color.Default.some))

  def noBg: ColorString =
    this.copy(_.copy(bg = None))

  // =====| ... |=====

  def toColorString: ColorString =
    this

  def +(other: ColorString): ColorString =
    this match {
      case ColorString.Simple(color, str) =>
        ColorString.Complex(color, (str.some, other) :: Nil, None)
      case ColorString.Complex(color, pairs, tail) =>
        ColorString.Complex(color, pairs :+ (tail, other), None)
    }

  def +(otherStr: String): ColorString =
    this match {
      case ColorString.Simple(color, str) =>
        ColorString.Simple(color, str + otherStr)
      case ColorString.Complex(color, pairs, tail) =>
        ColorString.Complex(color, pairs, tail.fold(otherStr)(_ + otherStr).some)
    }

  def split(splitStr: String): List[ColorString] = {
    val res =
      this match {
        case ColorString.Simple(color, str) =>
          str.split(splitStr).map(ColorString.Simple(color, _)).toList
        case ColorString.Complex(color, pairs, tail) =>
          List(
            pairs.flatMap { case (oStr, cStr) =>
              List(
                oStr.toList.flatMap {
                  _.split(splitStr).map(ColorString.Simple(color, _))
                },
                cStr.split(splitStr),
              ).flatten
            },
            tail.toList.flatMap {
              _.split(splitStr).map(ColorString.Simple(color, _))
            },
          ).flatten
      }

    res
  }

  def show: String =
    this match {
      case ColorString.Simple(color, str) =>
        s"Simple($color: ${str.unesc})"
      case ColorString.Complex(color, pairs, tail) =>
        def pairToStrings(pair: (Option[String], ColorString)): List[String] =
          List(pair._1.fold("_")(_.unesc), pair._2.show)

        val allElems: List[String] =
          pairs.flatMap(pairToStrings) ::: tail.fold("_")(_.unesc) :: Nil

        s"Complex($color: ${allElems.mkString(", ")})"
    }

  def length: Int =
    this match {
      case ColorString.Simple(_, str) =>
        str.length
      case ColorString.Complex(_, pairs, tail) =>
        pairs.map { case (oStr, cStr) =>
          oStr.fold(0)(_.length) + cStr.length
        }.sum + tail.fold(0)(_.length)
    }

  def toRawString: String = {
    val stringBuilder: StringBuilder = new StringBuilder

    def rec(
        colorString: ColorString,
    ): Unit =
      colorString match {
        case ColorString.Simple(_, str) =>
          stringBuilder.append(str)
        case ColorString.Complex(_, pairs, tail) =>
          pairs.foreach { case (str, cStr) =>
            str.foreach(stringBuilder.append)
            rec(cStr)
          }
          tail.foreach(stringBuilder.append)
      }

    rec(this)
    stringBuilder.toString
  }

  override def toString: String = toString(ColorMode.Extended)
  def toString(colorMode: ColorMode): String = {
    val stringBuilder: StringBuilder = new StringBuilder

    def append(
        prevColorState: ColorString.ColorState,
        newColorState: ColorString.ColorState,
        str: String,
    ): ColorString.ColorState = {
      val before = stringBuilder.toString
      val res =
        newColorState.diffWithState(prevColorState, colorMode) match {
          case Some((ansi, ncs)) =>
            stringBuilder.append(ansi).append(str)
            ncs
          case None =>
            stringBuilder.append(str)
            prevColorState
        }
      val after = stringBuilder.toString

      res
    }

    def rec(
        colorString: ColorString,
        prevColorState: ColorString.ColorState,
    ): ColorString.ColorState =
      colorString match {
        case ColorString.Simple(color, str) =>
          append(
            prevColorState,
            color.toColorState(prevColorState),
            str,
          )
        case ColorString.Complex(color, pairs, tail) =>
          val newColorState = color.toColorState(prevColorState)
          val afterPairs =
            pairs.foldLeft(prevColorState) { case (ccs, (oStr, cStr)) =>
              val afterOStr =
                oStr.fold(ccs)(append(ccs, newColorState, _))
              val afterCStr =
                rec(
                  cStr,
                  afterOStr,
                )

              afterCStr
            }
          val afterTail =
            tail.map(append(afterPairs, newColorState, _)).getOrElse(afterPairs)

          afterTail
      }

    val finalColorState = rec(
      this,
      ColorString.ColorState.Default,
    )
    append(finalColorState, ColorString.ColorState.Default, "")
    stringBuilder.toString
  }

}

object ColorString {
  import harness.core.Color as RawColor

  final case class Color(
      fg: Option[RawColor],
      bg: Option[RawColor],
  ) {

    def overwrite(other: Color): Color =
      Color(
        fg = other.fg.orElse(fg),
        bg = other.bg.orElse(bg),
      )

    def underwrite(other: Color): Color =
      Color(
        fg = fg.orElse(other.fg),
        bg = bg.orElse(other.bg),
      )

    def toColorState(parentColorState: ColorState): ColorState =
      ColorState(
        fg.getOrElse(parentColorState.fg),
        bg.getOrElse(parentColorState.bg),
      )

    override def toString: String =
      bg match {
        case Some(bg) =>
          fg match {
            case Some(fg) => s"$fg.fg + $bg.bg"
            case None     => s"$bg.bg"
          }
        case None =>
          fg match {
            case Some(fg) => s"$fg.fg"
            case None     => "NoColor"
          }
      }

  }
  object Color {
    val Empty: Color = Color(fg = None, bg = None)
    val Default: Color = Color(fg = RawColor.Default.some, bg = RawColor.Default.some)
  }

  final case class ColorState(
      fg: RawColor,
      bg: RawColor,
  ) {

    def toColor: Color = Color(fg = fg.some, bg = bg.some)

    def colorizeAndDeColorize(surroundings: ColorState, cm: ColorMode): Option[(String, String)] = {
      (this.fg != surroundings.fg, this.bg != surroundings.bg) match {
        case (true, true) =>
          (
            cm.ansiEscape(NonEmptyList.of(cm.fgMod(this.fg), cm.bgMod(this.bg))),
            cm.ansiEscape(NonEmptyList.of(cm.fgMod(surroundings.fg), cm.bgMod(surroundings.bg))),
          ).some
        case (true, false) =>
          (
            cm.ansiEscape(NonEmptyList.of(cm.fgMod(this.fg))),
            cm.ansiEscape(NonEmptyList.of(cm.fgMod(surroundings.fg))),
          ).some
        case (false, true) =>
          (
            cm.ansiEscape(NonEmptyList.of(cm.bgMod(this.bg))),
            cm.ansiEscape(NonEmptyList.of(cm.bgMod(surroundings.bg))),
          ).some
        case (false, false) =>
          None
      }
    }

    // Does a diff, to make sure any coloring is needed
    def diffWithState(colorState: ColorState, colorMode: ColorMode): Option[(String, ColorState)] =
      (Option.when(fg != colorState.fg)(fg), Option.when(bg != colorState.bg)(bg)) match {
        case (Some(fg), Some(bg)) =>
          (
            colorMode.ansiEscape(NonEmptyList.of(colorMode.fgMod(fg), colorMode.bgMod(bg))),
            colorState.copy(fg = fg, bg = bg),
          ).some
        case (Some(fg), None) =>
          (
            colorMode.ansiEscape(NonEmptyList.of(colorMode.fgMod(fg))),
            colorState.copy(fg = fg),
          ).some
        case (None, Some(bg)) =>
          (
            colorMode.ansiEscape(NonEmptyList.of(colorMode.bgMod(bg))),
            colorState.copy(bg = bg),
          ).some
        case (None, None) =>
          None
      }

  }
  object ColorState {
    val Default: ColorState = ColorState(RawColor.Default, RawColor.Default)
  }

  // =====| ... |=====

  final case class Simple(
      color: Color,
      str: String,
  ) extends ColorString {

    override def copy(cpF: Color => Color): ColorString =
      Simple(
        cpF(color),
        str,
      )

  }

  // color"someString${cString}${cString}someString${cString}someString"
  final case class Complex private[core] (
      color: Color,
      pairs: List[(Option[String], ColorString)],
      tail: Option[String],
  ) extends ColorString {

    override def copy(cpF: Color => Color): ColorString =
      Complex(
        cpF(color),
        pairs,
        tail,
      )

  }

}

given convertStringToColorString: Conversion[String, ColorString] = ColorString.Simple(ColorString.Color.Default, _)

implicit class ColorStringInterpolator(sc: StringContext) {

  def color(args: ColorString*): ColorString = {
    @tailrec
    def loop(
        sQueue: List[String],
        csQueue: List[ColorString],
        stack: List[(Option[String], ColorString)],
    ): (List[(Option[String], ColorString)], Option[String]) =
      (sQueue, csQueue) match {
        case (sH :: sT, csH :: csT) =>
          loop(
            sT,
            csT,
            (sH.toNES, csH) :: stack,
          )
        case (_, Nil) =>
          (stack.reverse, sQueue.mkString.toNES)
        case (Nil, csH :: csT) => // This should not be possible...
          loop(
            Nil,
            csT,
            (None, csH) :: stack,
          )
      }

    val (pairs, tail) = loop(sc.parts.toList, args.toList, Nil)

    ColorString.Complex(ColorString.Color.Empty, pairs, tail)
  }

}

extension (csl: List[ColorString]) {

  def csMkString: ColorString =
    csMkString("", "", "")

  def csMkString(sep: String): ColorString =
    csMkString("", sep, "")

  def csMkString(start: String, sep: String, end: String): ColorString = {
    val sepO = sep.toNES

    val pairs: List[(Option[String], ColorString)] =
      csl.toNel match {
        case Some(csl) => csl.tail.foldLeft((start.toNES, csl.head) :: Nil) { (l, cs) => (sepO, cs) :: l }.reverse
        case None      => Nil
      }

    ColorString.Complex(ColorString.Color.Empty, pairs, end.toNES)
  }

}
