package harness.csv

import cats.syntax.either.*
import cats.syntax.option.*
import cats.syntax.traverse.*
import harness.core.{StringDecoder, Zip}

trait CsvDecoder[T] private { self =>

  protected val size: Int
  protected def decodeImpl(line: IArray[Option[String]], lineNo: Int, cell: Int): Either[String, T]

  final def decode(line: IArray[Option[String]], lineNo: Int): Either[String, T] =
    if (line.length != self.size) s"Invalid line length on line $lineNo. Expected $size, but got ${line.length}".asLeft
    else self.decodeImpl(line, lineNo, 0)

  final def optional: CsvDecoder[Option[T]] =
    new CsvDecoder[Option[T]] {
      override protected val size: Int = self.size
      override protected def decodeImpl(line: IArray[Option[String]], lineNo: Int, cell: Int): Either[String, Option[T]] =
        if (cell.until(cell + self.size).forall(line(_).isEmpty)) None.asRight
        else self.decodeImpl(line, lineNo, cell).map(_.some)
    }

  final def ++[T2](other: CsvDecoder[T2])(implicit zip: Zip[T, T2]): CsvDecoder[zip.Out] =
    new CsvDecoder[zip.Out] {
      override protected val size: Int = self.size + other.size
      override protected def decodeImpl(line: IArray[Option[String]], lineNo: Int, cell: Int): Either[String, zip.Out] =
        for {
          t1 <- self.decodeImpl(line, lineNo, cell)
          t2 <- other.decodeImpl(line, lineNo, cell + self.size)
        } yield zip.zip(t1, t2)
    }

  final def apply[T2](implicit other: CsvDecoder[T2], zip: Zip[T, T2]): CsvDecoder[zip.Out] = self ++ other

  final def map[T2](f: T => T2): CsvDecoder[T2] =
    new CsvDecoder[T2] {
      override protected val size: Int = self.size
      override protected def decodeImpl(line: IArray[Option[String]], lineNo: Int, cell: Int): Either[String, T2] =
        self.decodeImpl(line, lineNo, cell).map(f)
    }

  final def emap[T2](f: T => Either[String, T2]): CsvDecoder[T2] =
    new CsvDecoder[T2] {
      override protected val size: Int = self.size
      override protected def decodeImpl(line: IArray[Option[String]], lineNo: Int, cell: Int): Either[String, T2] =
        self.decodeImpl(line, lineNo, cell).flatMap(f)
    }

}
object CsvDecoder {

  inline def apply[T](implicit decoder: CsvDecoder[T]): CsvDecoder[T] = decoder

  def decode[T](csv: String, hasHeaders: Boolean)(implicit decoder: CsvDecoder[T]): Either[String, List[T]] =
    for {
      lines <- Parser.parse(csv)
      (lines, startLineNo) <- (hasHeaders, lines) match {
        case (false, lines)     => (lines, 1).asRight
        case (true, _ :: lines) => (lines, 2).asRight
        case (true, Nil)        => "No header line to ignore".asLeft
      }
      results <- lines.zipWithIndex.traverse { (line, idx) => decoder.decode(line, idx + startLineNo) }
    } yield results

  def cell[T](f: String => Either[String, T]): CsvDecoder[T] =
    new CsvDecoder[T] {
      override protected val size: Int = 1
      override protected def decodeImpl(line: IArray[Option[String]], lineNo: Int, cell: Int): Either[String, T] = {
        line(cell) match {
          case Some(value) => f(value).leftMap(err => s"Error on line $lineNo in cell $cell ${cell + 1} : $err")
          case None        => s"Error on line $lineNo in cell $cell ${cell + 1} : unexpected null".asLeft
        }
      }
    }

  implicit val unitDecoder: CsvDecoder[Unit] =
    new CsvDecoder[Unit] {
      override protected val size: Int = 1
      override protected def decodeImpl(line: IArray[Option[String]], lineNo: Int, cell: Int): Either[String, Unit] = ().asRight
    }

  implicit def optional[T](implicit decoder: CsvDecoder[T]): CsvDecoder[Option[T]] = decoder.optional

  implicit def fromStringDecoder[T](implicit decoder: StringDecoder[T]): CsvDecoder[T] = CsvDecoder.cell(decoder.decode)

}
