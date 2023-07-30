package harness.core

import cats.data.*
import cats.syntax.either.*
import cats.syntax.parallel.*
import cats.syntax.traverse.*
import java.time.*
import java.util.UUID
import scala.reflect.ClassTag
import scala.util.Try

trait StringDecoder[+T] { self =>

  def decodeAccumulating(str: String): EitherNel[String, T]

  final def decode(string: String): Either[String, T] = decodeAccumulating(string).leftMap(_.head)

  final def map[T2](f: T => T2): StringDecoder[T2] =
    self.decodeAccumulating(_).map(f)

  final def flatMap[T2](f: T => EitherNel[String, T2]): StringDecoder[T2] =
    self.decodeAccumulating(_).flatMap(f)

}
object StringDecoder {

  def apply[T: StringDecoder]: StringDecoder[T] =
    implicitly[StringDecoder[T]]

  def fromOptionF[R](typeName: String, f: String => Option[R]): StringDecoder[R] =
    str => f(str).toRight(NonEmptyList.one(s"Malformatted $typeName '$str'"))

  def fromTryF[R](typeName: String, f: String => R): StringDecoder[R] =
    fromOptionF(typeName, string => Try(f(string)).toOption)

  implicit val string: StringDecoder[String] =
    _.asRight

  implicit val boolean: StringDecoder[Boolean] =
    fromOptionF("boolean", _.toBooleanOption)

  implicit val int: StringDecoder[Int] =
    fromOptionF("int", _.toIntOption)

  implicit val long: StringDecoder[Long] =
    fromOptionF("long", _.toLongOption)

  implicit val bigInt: StringDecoder[BigInt] =
    fromTryF("bigInt", BigInt(_))

  implicit val float: StringDecoder[Float] =
    fromOptionF("float", _.toFloatOption)

  implicit val double: StringDecoder[Double] =
    fromOptionF("double", _.toDoubleOption)

  implicit val bigDecimal: StringDecoder[BigDecimal] =
    fromTryF("bigDecimal", BigDecimal(_))

  implicit val uuid: StringDecoder[UUID] =
    fromTryF("uuid", UUID.fromString)

  implicit val duration: StringDecoder[Duration] =
    fromTryF("duration", Duration.parse)

  def list[T](sep: String)(implicit tDecoder: StringDecoder[T]): StringDecoder[List[T]] =
    _.split(sep).toList.parTraverse(tDecoder.decodeAccumulating)

  implicit def list[T: StringDecoder]: StringDecoder[List[T]] =
    list[T](",")

  private object temporal {
    val numsOneTwo = "(\\d{1,2})"
    val numsTwo = "(\\d{2})"
    val numsFour = "(\\d{4})"
    val someSpacing = "\\s+".r
  }

  def configurableLocalDate(_currentYear: => Int, futureTolerance: => Int): StringDecoder[LocalDate] = { str =>
    import temporal.*

    val us2Year = s"$numsOneTwo/$numsOneTwo/$numsTwo".r
    val us4Year = s"$numsOneTwo/$numsOneTwo/$numsFour".r
    val other2Year = s"$numsOneTwo\\.$numsOneTwo\\.$numsTwo".r
    val other4Year = s"$numsOneTwo\\.$numsOneTwo\\.$numsFour".r
    val yearFirst = s"$numsFour-$numsOneTwo-$numsOneTwo".r

    def guessYear(y: Int): Int = {
      val currentYear = _currentYear
      val currentCentury = currentYear / 100
      val currentYearInCentury = currentYear % 100
      val futureYearInCentury = currentYearInCentury + futureTolerance
      val centuryGuess =
        if (futureYearInCentury >= 100)
          if (y >= currentYearInCentury) currentCentury
          else if (y <= futureYearInCentury % 100) currentCentury + 1
          else currentCentury
        else if (y <= futureYearInCentury) currentCentury
        else currentCentury - 1

      centuryGuess * 100 + y
    }

    def attemptDate(date: => LocalDate): EitherNel[String, LocalDate] =
      Try(date).toEither.leftMap(_.getMessage).toEitherNel

    str match {
      case us2Year(month, day, year)    => attemptDate(LocalDate.of(guessYear(year.toInt), month.toInt, day.toInt))
      case us4Year(month, day, year)    => attemptDate(LocalDate.of(year.toInt, month.toInt, day.toInt))
      case other2Year(day, month, year) => attemptDate(LocalDate.of(guessYear(year.toInt), month.toInt, day.toInt))
      case other4Year(day, month, year) => attemptDate(LocalDate.of(year.toInt, month.toInt, day.toInt))
      case yearFirst(year, month, day)  => attemptDate(LocalDate.of(year.toInt, month.toInt, day.toInt))
      case _                            => s"Malformatted date '$str'".leftNel
    }
  }

  implicit val localDate: StringDecoder[LocalDate] = configurableLocalDate(LocalDate.now.getYear, 10)

  implicit val localTime: StringDecoder[LocalTime] = { str =>
    import temporal.*

    val hourMinute = s"$numsOneTwo:$numsTwo".r
    val hourMinuteAM = s"$numsOneTwo:$numsTwo$someSpacing(?:AM|am)".r
    val hourMinutePM = s"$numsOneTwo:$numsTwo$someSpacing(?:PM|pm)".r
    val hourMinuteSecond = s"$numsOneTwo:$numsTwo$numsTwo".r
    val hourMinuteSecondAM = s"$numsOneTwo:$numsTwo$numsTwo$someSpacing(?:AM|am)".r
    val hourMinuteSecondPM = s"$numsOneTwo:$numsTwo$numsTwo$someSpacing(?:PM|pm)".r

    def attemptTime(time: => LocalTime): EitherNel[String, LocalTime] =
      Try(time).toEither.leftMap(_.getMessage).toEitherNel

    def pmTime(hour: Int): Int =
      if (hour == 12) 0
      else hour + 12

    str match {
      case hourMinute(hour, minute)                 => attemptTime(LocalTime.of(hour.toInt, minute.toInt))
      case hourMinuteAM(hour, minute)               => attemptTime(LocalTime.of(hour.toInt, minute.toInt))
      case hourMinutePM(hour, minute)               => attemptTime(LocalTime.of(pmTime(hour.toInt), minute.toInt))
      case hourMinuteSecond(hour, minute, second)   => attemptTime(LocalTime.of(hour.toInt, minute.toInt, second.toInt))
      case hourMinuteSecondAM(hour, minute, second) => attemptTime(LocalTime.of(hour.toInt, minute.toInt, second.toInt))
      case hourMinuteSecondPM(hour, minute, second) => attemptTime(LocalTime.of(pmTime(hour.toInt), minute.toInt, second.toInt))
      case _                                        => s"Malformatted time '$str'".leftNel
    }
  }

  def configurableLocalDateTime(_currentYear: => Int, futureTolerance: => Int): StringDecoder[LocalDateTime] = { str =>
    val splitStr = "\\s+(?!(\\s|AM|PM|am|pm))"

    str.split(splitStr) match {
      case Array(str1, str2) =>
        (configurableLocalDate(_currentYear, futureTolerance).decodeAccumulating(str1), localTime.decodeAccumulating(str2)).parTupled match {
          case Right((date, time)) => LocalDateTime.of(date, time).asRight
          case Left(e1) =>
            (localTime.decodeAccumulating(str1), configurableLocalDate(_currentYear, futureTolerance).decodeAccumulating(str2)).parTupled match {
              case Right((time, date)) => LocalDateTime.of(date, time).asRight
              case Left(e2) =>
                NonEmptyList
                  .of(
                    s"Could not parse 'DATE TIME' : ${e1.toList.mkString(", ")}",
                    s"Could not parse 'TIME DATE' : ${e2.toList.mkString(", ")}",
                  )
                  .asLeft
            }
        }
      case Array(str1) => localDate.decodeAccumulating(str1).map(_.atStartOfDay)
      case _           => s"Malformatted date-time '$str'".leftNel
    }
  }

  implicit val localDateTimeDecodeString: StringDecoder[LocalDateTime] = configurableLocalDateTime(LocalDate.now.getYear, 10)

  def `enum`[E <: Enum[E], Enc: StringDecoder](implicit ewe: Enum.WithEnc[E, Enc], ct: ClassTag[E]): StringDecoder[E] =
    StringDecoder[Enc].flatMap { enc =>
      ewe.decode(enc) match {
        case Some(value) => value.asRight
        case None        => s"Invalid encoding for enum ${ct.runtimeClass.getSimpleName}: '$enc'".leftNel
      }
    }

}
