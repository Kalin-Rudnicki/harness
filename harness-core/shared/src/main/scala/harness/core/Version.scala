package harness.core

import cats.data.NonEmptyList
import cats.syntax.option.*
import scala.annotation.tailrec

final case class Version(
    hasVPrefix: Boolean,
    numbers: NonEmptyList[Int],
    suffixOpt: Option[String],
) {
  private val numbersList: List[Int] = numbers.toList
  private val numbersArray: IArray[Int] = IArray.from(numbersList)

  final def major: Int = numbersArray(0)

  final def minorOpt: Option[Int] = Option.when(numbersArray.length >= 2)(numbersArray(1))
  final def minor: Int = minorOpt.getOrElse(0)

  final def patchOpt: Option[Int] = Option.when(numbersArray.length >= 3)(numbersArray(2))
  final def patch: Int = minorOpt.getOrElse(0)

  final def strippedSuffixOpt: Option[String] = suffixOpt.map(_.toList.dropWhile(_ == '-').mkString)
  final def strippedSuffix: String = strippedSuffixOpt.getOrElse("")

  override def toString: String = numbersList.mkString(if (hasVPrefix) "v" else "", ".", suffixOpt.getOrElse(""))

}
object Version {

  private val reg = "^(v)?([0-9]+(?:\\.[0-9]+)*)(-.+)?$".r

  implicit val stringEncoder: StringEncoder[Version] = StringEncoder.usingToString
  implicit val stringDecoder: StringDecoder[Version] = StringDecoder.fromOptionF("Version", Version.parse)
  implicit val ordering: Ordering[Version] = { (versionA, versionB) =>
    @tailrec
    def loop(a: List[Int], b: List[Int]): Int =
      (a, b) match {
        case (aHead :: aTail, bHead :: bTail) =>
          if (aHead < bHead) -1
          else if (aHead > bHead) 1
          else loop(aTail, bTail)
        case (_ :: _, Nil) => 1
        case (Nil, _ :: _) => -1
        case (Nil, Nil) =>
          (versionA.suffixOpt, versionB.suffixOpt) match {
            case (None, None)                   => 0
            case (Some(aSuffix), Some(bSuffix)) => Ordering[String].compare(aSuffix, bSuffix)
            case (Some(_), None)                => -1
            case (None, Some(_))                => 1
          }
      }

    loop(versionA.numbersList, versionB.numbersList)
  }

  def parse(string: String): Option[Version] =
    string match {
      case reg(vPrefix, numbers, suffix) => Version(vPrefix != null, NonEmptyList.fromListUnsafe(numbers.split("\\.").toList.map(_.toInt)), Option(suffix)).some
      case _                             => None
    }

  def parseUnsafe(string: String): Version =
    parse(string) match {
      case Some(version) => version
      case None          => throw new RuntimeException(s"Invalid version: '$string'")
    }

}
