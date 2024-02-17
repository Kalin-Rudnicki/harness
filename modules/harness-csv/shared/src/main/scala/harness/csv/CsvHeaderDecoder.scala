package harness.csv

import cats.data.NonEmptyList
import cats.syntax.either.*
import cats.syntax.option.*
import cats.syntax.traverse.*
import harness.core.{StringDecoder, Zip}

trait CsvHeaderDecoder[T] private { self =>

  protected def buildImpl(headers: IArray[Option[String]]): Either[String, CsvHeaderDecoder.AppliedCsvHeaderDecoder[T]]

  final def decode(lines: List[IArray[Option[String]]]): Either[String, List[T]] =
    lines match {
      // TODO (KR) :
      case head :: tail =>
        self.buildImpl(head).flatMap { decoder =>
          tail.zipWithIndex.traverse { (line, idx) => decoder.decodeImpl(line, idx + 2) }
        }
      case Nil => "Missing header row".asLeft
    }

  final def optional: CsvHeaderDecoder[Option[T]] =
    self.buildImpl(_).map(_.optional)

  final def ++[T2](other: CsvHeaderDecoder[T2])(implicit zip: Zip[T, T2]): CsvHeaderDecoder[zip.Out] = { headers =>
    for {
      t <- self.buildImpl(headers)
      t2 <- other.buildImpl(headers)
    } yield t ++ t2
  }

  final def map[T2](f: T => T2): CsvHeaderDecoder[T2] =
    self.buildImpl(_).map(_.map(f))

  final def emap[T2](f: T => Either[String, T2]): CsvHeaderDecoder[T2] =
    self.buildImpl(_).map(_.emap(f))

}
object CsvHeaderDecoder {

  inline def apply[T](implicit decoder: CsvHeaderDecoder[T]): CsvHeaderDecoder[T] = decoder

  def decode[T](csv: String)(implicit decoder: CsvHeaderDecoder[T]): Either[String, List[T]] =
    Parser.parse(csv).flatMap(decoder.decode)

  def cell[T](header: String, f: String => Either[String, T]): CsvHeaderDecoder[T] =
    _.zipWithIndex.find(_._1.contains(header)) match {
      case Some((_, cell)) =>
        new AppliedCsvHeaderDecoder[T] {
          override protected val minSize: Int = cell
          override protected val headerIndexes: Set[Int] = Set(cell)
          override def decodeImpl(line: IArray[Option[String]], lineNo: Int): Either[String, T] =
            line(cell) match {
              case Some(value) => f(value)
              case None        => s"Error for header '$header' on line $lineNo in cell ${cell + 1}  : unexpected null".asLeft
            }
        }.asRight
      case None => s"Could not find header '$header'".asLeft
    }

  def cell[T](header: String)(implicit decoder: StringDecoder[T]): CsvHeaderDecoder[T] =
    CsvHeaderDecoder.cell(header, decoder.decode)

  def multiOptHeaderDecoder[T](header: String)(implicit decoder: StringDecoder[T]): CsvHeaderDecoder[NonEmptyList[Option[T]]] = { headers =>
    NonEmptyList.fromList(headers.zipWithIndex.filter { (h, _) => h.contains(header) }.map(_._2).toList) match {
      case Some(indexes) =>
        new AppliedCsvHeaderDecoder[NonEmptyList[Option[T]]] {
          override protected val minSize: Int = indexes.toList.max
          override protected val headerIndexes: Set[Int] = indexes.toList.toSet
          override def decodeImpl(line: IArray[Option[String]], lineNo: Int): Either[String, NonEmptyList[Option[T]]] =
            indexes.traverse { line(_).traverse(decoder.decode) }
        }.asRight
      case None => s"Did not find any headers with value '$header'".asLeft
    }
  }

  implicit def optional[T](implicit decoder: CsvHeaderDecoder[T]): CsvHeaderDecoder[Option[T]] =
    decoder.optional

  trait AppliedCsvHeaderDecoder[T] { self =>

    protected val minSize: Int
    protected val headerIndexes: Set[Int]
    def decodeImpl(line: IArray[Option[String]], lineNo: Int): Either[String, T]

    final def optional: CsvHeaderDecoder.AppliedCsvHeaderDecoder[Option[T]] =
      new AppliedCsvHeaderDecoder[Option[T]] {
        override protected val minSize: Int = self.minSize
        override protected val headerIndexes: Set[Int] = self.headerIndexes
        override def decodeImpl(line: IArray[Option[String]], lineNo: Int): Either[String, Option[T]] =
          if (self.headerIndexes.forall(line(_).isEmpty)) None.asRight
          else self.decodeImpl(line, lineNo).map(_.some)
      }

    final def ++[T2](other: CsvHeaderDecoder.AppliedCsvHeaderDecoder[T2])(implicit zip: Zip[T, T2]): CsvHeaderDecoder.AppliedCsvHeaderDecoder[zip.Out] =
      new AppliedCsvHeaderDecoder[zip.Out] {
        override protected val minSize: Int = self.minSize.max(other.minSize)
        override protected val headerIndexes: Set[Int] = self.headerIndexes ++ other.headerIndexes
        override def decodeImpl(line: IArray[Option[String]], lineNo: Int): Either[String, zip.Out] =
          for {
            t <- self.decodeImpl(line, lineNo)
            t2 <- other.decodeImpl(line, lineNo)
          } yield zip.zip(t, t2)
      }

    final def map[T2](f: T => T2): CsvHeaderDecoder.AppliedCsvHeaderDecoder[T2] =
      new AppliedCsvHeaderDecoder[T2] {
        override protected val minSize: Int = self.minSize
        override protected val headerIndexes: Set[Int] = self.headerIndexes
        override def decodeImpl(line: IArray[Option[String]], lineNo: Int): Either[String, T2] =
          self.decodeImpl(line, lineNo).map(f)
      }

    final def emap[T2](f: T => Either[String, T2]): CsvHeaderDecoder.AppliedCsvHeaderDecoder[T2] =
      new AppliedCsvHeaderDecoder[T2] {
        override protected val minSize: Int = self.minSize
        override protected val headerIndexes: Set[Int] = self.headerIndexes
        override def decodeImpl(line: IArray[Option[String]], lineNo: Int): Either[String, T2] =
          self.decodeImpl(line, lineNo).flatMap(f)
      }

  }

}
