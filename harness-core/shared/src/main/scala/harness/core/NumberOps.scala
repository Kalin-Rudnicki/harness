package harness.core

private def commaify(str: String): String = {
  def placeCommas(chars: List[Char]): String =
    chars.reverse.grouped(3).toList.reverseIterator.map(_.reverse.mkString).mkString(",")

  str.toList match {
    case '-' :: chars => "-" + placeCommas(chars)
    case chars        => placeCommas(chars)
  }
}

extension (self: Long) {

  def toStringCommas: String =
    commaify(self.toString)

  def pluralizeOn(base: String, pluralSuffix: String = "s", singularSuffix: String = "", addCommas: Boolean = true): String =
    s"${if (addCommas) self.toStringCommas else self.toString} ${base.pluralize(self, pluralSuffix, singularSuffix)}"

}

extension (self: Double) {

  def toStringCommas(showEmptyDecimal: Boolean): String =
    BigDecimal(self).toStringCommas(showEmptyDecimal)

  def roundTo(numPlaces: Int): Double =
    roundTo(Math.pow(10, numPlaces))

  def roundTo(mult: Double): Double =
    (self * mult).round.toDouble / mult

  def floorTo(mult: Double): Double =
    (self * mult).floor / mult

  def ceilTo(mult: Double): Double =
    (self * mult).ceil / mult

}

extension (self: BigDecimal) {

  def toStringCommas(showEmptyDecimal: Boolean): String = {
    def convert(int: String, fract: String): String =
      (commaify(int) :: Option.when(showEmptyDecimal || fract != "0")(fract).toList).mkString(".")

    self.toString.split("\\.") match {
      case Array(int, fract) => convert(int, fract)
      case Array(int)        => convert(int, "0")
      case _                 => ???
    }
  }

}
