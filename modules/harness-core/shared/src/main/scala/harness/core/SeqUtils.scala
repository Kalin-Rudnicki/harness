package harness.core

import cats.Monoid
import cats.syntax.either.*

object SeqUtils {

  /**
    * Assumes all lists have same size, will throw if this is not the case
    */
  def unsafeTranspose[A](seqs: Seq[Seq[A]]): Seq[Seq[A]] =
    seqs.map(_.length).maxOption match {
      case Some(len) => 0.until(len).map { i => seqs.map(_(i)) }
      case None      => seqs
    }

  def transposeOrFill[A](seqs: Seq[Seq[A]])(implicit monoid: Monoid[A]): Seq[Seq[A]] =
    seqs.map(_.length).maxOption match {
      case Some(len) => 0.until(len).map { i => seqs.map(_.lift(i).getOrElse(monoid.empty)) }
      case None      => seqs
    }

  def align(alignments: Seq[Alignment], seqs: Seq[Seq[String]]): Seq[Seq[String]] = {
    val columns = transposeOrFill(seqs)
    val alteredAlignments =
      if (alignments.length < columns.length) alignments ++ Seq.fill(columns.length - alignments.length)(Alignment.Left)
      else alignments
    val aligned = alteredAlignments.zip(columns).map { case (alignment, row) =>
      val maxLen = row.map(_.length).maxOption.getOrElse(0)
      row.map(_.align(maxLen, alignment))
    }
    unsafeTranspose(aligned)
  }

  def align(alignment: Alignment, seqs: Seq[Seq[String]]): Seq[Seq[String]] =
    seqs.headOption match {
      case Some(head) => align(head.map(_ => alignment), seqs)
      case None       => seqs
    }

  /**
    * Converts something like:
    *   List(
    *     List("A", "B", "C"),
    *     List("A", "B"),
    *     List("A"),
    *   )
    * into:
    *   List(
    *     List("A", "B", "C"),
    *     List("A", "B", "" ),
    *     List("A", "",  "" ),
    *   )
    */
  def fillSeqsToSameLengths[A](seqs: Seq[Seq[A]])(implicit monoid: Monoid[A]): Seq[Seq[A]] =
    seqs.map(_.length).maxOption match {
      case Some(len) =>
        seqs.map { s =>
          if (s.length < len) s ++ Seq.fill(len - s.length)(monoid.empty)
          else s
        }
      case None => seqs
    }

  def makeTable[Input, Headers <: Tuple, Row <: Tuple, HeaderAlignments <: Tuple, RowAlignments <: Tuple](
      inputs: Seq[Input],
  )(
      headers: Headers,
      headerAlignments: HeaderAlignments,
      rowAlignments: RowAlignments,
      padding: Int = 1,
  )(
      makeRow: Input => Row,
  )(implicit
      headersAreStrings: Tuple.Union[Headers] =:= String,
      headerAlignmentsAreAlignments: Tuple.Union[HeaderAlignments] =:= Alignment,
      rowAlignmentsAreAlignments: Tuple.Union[RowAlignments] =:= Alignment,
      rowSameSizeAsHeaders: Tuple.Size[Row] =:= Tuple.Size[Headers],
      headerAlignmentsSameSizeAsHeaders: Tuple.Size[HeaderAlignments] =:= Tuple.Size[Headers],
      rowAlignmentsSameSizeAsRows: Tuple.Size[RowAlignments] =:= Tuple.Size[Row],
  ): String = {
    val rowSeqs = unsafeTranspose(inputs.map(r => makeRow(r).toIArray.toIndexedSeq).asInstanceOf[Seq[Seq[String]]])
    val headerSeq = headers.toIArray.toIndexedSeq.asInstanceOf[Seq[String]]
    val headerAlignmentsSeq = headerAlignments.toIArray.toIndexedSeq.asInstanceOf[Seq[Alignment]]
    val rowAlignmentsSeq = rowAlignments.toIArray.toIndexedSeq.asInstanceOf[Seq[Alignment]]

    val maxLens = rowSeqs.map(_.map(_.length).maxOption.getOrElse(0)).zip(headerSeq).map { case (m, h) => h.length.max(m) }
    val alignedRows = maxLens.zip(rowSeqs).zip(rowAlignmentsSeq).map { case ((maxLen, row), alignment) => row.map(_.align(maxLen, alignment)) }
    val rowsFlippedBack = unsafeTranspose(alignedRows)

    val paddingStr = " " * padding
    val sepRow = maxLens.map(m => "-" * (m + 2 * padding)).mkString("+", "+", "+")
    val headerRow = maxLens.zip(headerSeq).zip(headerAlignmentsSeq).map { case ((m, h), a) => h.align(m, a) }.mkString(s"|$paddingStr", s"$paddingStr|$paddingStr", s"$paddingStr|")
    val joinedRows = rowsFlippedBack.map(_.mkString(s"|$paddingStr", s"$paddingStr|$paddingStr", s"$paddingStr|"))

    Seq(
      Seq(sepRow, headerRow, sepRow),
      joinedRows,
      Seq(sepRow),
    ).flatten.mkString("\n")
  }

}
