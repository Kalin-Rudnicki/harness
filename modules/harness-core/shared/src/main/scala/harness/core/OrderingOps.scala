package harness.core

import scala.Ordering.Implicits.infixOrderingOps

extension [A](self: A) {

  def showMagnitudeColorized(show: A => String)(default: Color, steps: (A, Color)*)(implicit ord: Ordering[A]): ColorString =
    show(self).fg(steps.filter(_._1 <= self).maxByOption(_._1).fold(default)(_._2))

  def showMagnitudeColorized(show: A => String)(steps: (A, Color)*)(implicit ord: Ordering[A]): ColorString =
    steps.filter(_._1 <= self).maxByOption(_._1) match
      case Some((_, color)) => show(self).fg(color)
      case None             => show(self).toColorString

}
