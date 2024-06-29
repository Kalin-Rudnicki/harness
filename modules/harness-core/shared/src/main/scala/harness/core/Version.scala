package harness.core

import cats.data.NonEmptyList
import cats.syntax.option.*
import scala.annotation.tailrec

final case class Version(
    hasVPrefix: Boolean,
    numbers: NonEmptyList[Int],
    suffixOpt: Option[Version.Suffix],
) {
  private val numbersList: List[Int] = numbers.toList

  def major: Int = numbers.head

  def minorOpt: Option[Int] = numbersList match
    case _ :: v :: _ => v.some
    case _           => None
  def minor: Int = minorOpt.getOrElse(0)

  def patchOpt: Option[Int] = numbersList match
    case _ :: _ :: v :: _ => v.some
    case _                => None
  def patch: Int = minorOpt.getOrElse(0)

  def suffixString: String = suffixOpt.fold("")(s => "-" + s.toString)

  override def toString: String = numbersList.mkString(if (hasVPrefix) "v" else "", ".", suffixString)

  override def equals(obj: Any): Boolean =
    obj.asInstanceOf[Matchable] match
      case that: Version => Version.ordering.equiv(this, that)
      case _             => false

}
object Version {

  val zero: Version = Version.make(0)

  final case class Suffix private (raw: String, repr: Suffix.Repr) {

    override def toString: String = raw

    override def equals(obj: Any): Boolean =
      obj.asInstanceOf[Matchable] match
        case that: Version.Suffix => Version.Suffix.ordering.equiv(this, that)
        case _                    => false

  }
  object Suffix {

    def fromRepr(repr: Suffix.Repr): Suffix = Suffix(repr.toString, repr)
    def snapshot: Suffix = Repr.Snapshot(None).toSuffix
    def snapshot(suffix: String): Suffix = Repr.Snapshot(suffix.some).toSuffix
    def rc(rc: Int): Suffix = Repr.RC(rc).toSuffix
    def other(other: String): Suffix = Repr.Other(other).toSuffix

    sealed trait Repr {

      override final def toString: String = this match
        case Repr.Snapshot(suffix) => "SNAPSHOT" + suffix.getOrElse("")
        case Repr.RC(rc)           => s"RC$rc"
        case Repr.Other(other)     => other

      override def equals(obj: Any): Boolean =
        obj.asInstanceOf[Matchable] match
          case that: Version.Suffix.Repr => Version.Suffix.Repr.ordering.equiv(this, that)
          case _                         => false

      final def toSuffix: Suffix = Suffix(this.toString, this)

    }
    object Repr {

      final case class Snapshot(suffix: Option[String]) extends Suffix.Repr
      final case class RC(rc: Int) extends Suffix.Repr
      final case class Other(other: String) extends Suffix.Repr

      private val getInt: Unapply[String, Int] = _.stripPrefix("-").toIntOption

      private def ord(suffix: Repr): Int = suffix match {
        case Snapshot(_) => 1
        case RC(_)       => 2
        case Other(_)    => 3
      }

      implicit val ordering: Ordering[Repr] = {
        case (Snapshot(a), Snapshot(b)) =>
          (a, b) match {
            case (Some(getInt(a)), Some(getInt(b))) => Ordering[Int].compare(a, b)
            case (Some(a), Some(b))                 => Ordering[String].compare(a, b)
            case (Some(_), None)                    => 1
            case (None, Some(_))                    => -1
            case (None, None)                       => 0
          }
        case (RC(a), RC(b))       => Ordering[Int].compare(a, b)
        case (Other(a), Other(b)) => Ordering[String].compare(a, b)
        case (a, b)               => Ordering[Int].compare(ord(a), ord(b))
      }

      private val rcReg = "^RC-?(\\d+)$".r
      private val snapshotReg1 = "^SNAPSHOT(.+)?$".r
      private val snapshotReg2 = "^SNAP([^A-Z].*)$".r

      def parse(suffix: String): Repr =
        suffix.toUpperCase match {
          case rcReg(rc)            => RC(rc.toInt)
          case snapshotReg1(suffix) => Snapshot(Option(suffix))
          case "SNAP"               => Snapshot(None)
          case snapshotReg2(suffix) => Snapshot(suffix.some)
          case suffix               => Other(suffix)
        }

    }

    export Suffix.Repr.{Other, RC, Snapshot}

    def parse(suffix: String): Suffix = Suffix(suffix, Repr.parse(suffix))

    implicit val ordering: Ordering[Suffix] = Ordering.by(_.repr)

  }

  private val reg = "^(v)?([0-9]+(?:\\.[0-9]+)*)(?:-(.+))?$".r

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
        case (rest @ _ :: _, Nil) if rest.exists(_ != 0) => 1
        case (Nil, rest @ _ :: _) if rest.exists(_ != 0) => -1
        case (_, _) =>
          (versionA.suffixOpt, versionB.suffixOpt) match {
            case (None, None)                   => 0
            case (Some(aSuffix), Some(bSuffix)) => Ordering[Suffix].compare(aSuffix, bSuffix)
            case (Some(_), None)                => -1
            case (None, Some(_))                => 1
          }
      }

    loop(versionA.numbersList, versionB.numbersList)
  }

  def make(n0: Int, nN: Int*): Version = Version(false, NonEmptyList(n0, nN.toList), None)
  def makeV(n0: Int, nN: Int*): Version = Version(true, NonEmptyList(n0, nN.toList), None)

  def parse(string: String): Option[Version] =
    string match {
      case reg(vPrefix, numbers, suffix) => Version(vPrefix != null, NonEmptyList.fromListUnsafe(numbers.split("\\.").toList.map(_.toInt)), Option(suffix).map(Suffix.parse)).some
      case _                             => None
    }

  def parseUnsafe(string: String): Version =
    parse(string) match {
      case Some(version) => version
      case None          => throw new RuntimeException(s"Invalid version: '$string'")
    }

  val fromString: Unapply[String, Version] = parse(_)

}
