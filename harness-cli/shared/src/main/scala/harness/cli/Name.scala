package harness.cli

import cats.syntax.either.*
import scala.reflect.ClassTag
import scala.util.matching.Regex

// TODO (KR) : should `showName` be "a"/"b-c" or "-a"/"--b-c"?
sealed abstract class Name(final val showName: String)

// TODO (KR) : NonEmptyList[Segment] for auto-complete purposes
final case class LongName private (firstChar: ShortName, name: String) extends Name(name) { self =>

  def -(other: LongName): LongName = new LongName(self.firstChar, s"${self.name}-${other.name}")

  override final def hashCode: Int = self.name.hashCode
  override final def equals(other: Any): Boolean =
    other.asInstanceOf[Matchable] match {
      case other: LongName => self.name == other.name
      case _               => false
    }
  override final def toString: String = self.name
}
object LongName { self =>

  private val segmentReg: Regex = s"${ShortName.regex}+".r
  val regex: Regex = s"$segmentReg(?:-$segmentReg)*".r

  def apply(name: String): Either[String, LongName] =
    if (regex.matches(name)) new LongName(ShortName.unsafe(name.head), name).asRight
    else s"Invalid LongName '$name'".asLeft

  // NOTE : this can throw
  def unsafe(name: String): LongName =
    self(name) match {
      case Right(value)  => value
      case Left(message) => throw new RuntimeException(message)
    }

}

sealed abstract class ShortName(final val name: Char) extends Name(name.toString) { self =>
  override final def hashCode: Int = self.name.hashCode
  override final def equals(other: Any): Boolean =
    other.asInstanceOf[Matchable] match {
      case other: ShortName => self.name == other.name
      case _                => false
    }
  override final def toString: String = self.name.toString
}
object ShortName
    extends ShortNameBuilder[ShortName]({
      case n if n.isLower => new ShortName.LowerLetter(n)
      case n if n.isUpper => new ShortName.UpperLetter(n)
      case n if n.isDigit => new ShortName.Digit(n)
    }) {

  val regex: Regex = "[A-Za-z0-9]".r

  // =====| Types |=====

  sealed abstract class Letter(name: Char) extends ShortName(name) {
    final def toLower: LowerLetter = LowerLetter.unsafe(name.toLower)
    final def toUpper: UpperLetter = UpperLetter.unsafe(name.toUpper)
  }
  object Letter
      extends ShortNameBuilder[Letter]({
        case n if n.isLower => new LowerLetter(n)
        case n if n.isUpper => new UpperLetter(n)
      })

  final class LowerLetter private[ShortName] (name: Char) extends Letter(name)
  object LowerLetter extends ShortNameBuilder[LowerLetter]({ case n if n.isLower => new LowerLetter(n) })

  final class UpperLetter private[ShortName] (name: Char) extends Letter(name)
  object UpperLetter extends ShortNameBuilder[UpperLetter]({ case n if n.isUpper => new UpperLetter(n) })

  final class Digit private[ShortName] (name: Char) extends ShortName(name)
  object Digit extends ShortNameBuilder[Digit]({ case n if n.isDigit => new Digit(n) })

}

sealed abstract class ShortNameBuilder[T <: ShortName](final val partial: PartialFunction[Char, T])(implicit ct: ClassTag[T]) { self =>

  final def apply(name: Char): Either[String, T] =
    partial.lift(name).toRight(s"Invalid ${ct.runtimeClass.getSimpleName} '$name'")

  // NOTE : this can throw
  final def unsafe(name: Char): T =
    self(name) match {
      case Right(value)  => value
      case Left(message) => throw new RuntimeException(message)
    }

}
