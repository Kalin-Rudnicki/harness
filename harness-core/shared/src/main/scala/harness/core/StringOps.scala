package harness.core

import cats.syntax.option.*

extension (self: String) {

  // =====| Misc |=====

  def toNES: Option[String] =
    Option.when(self.nonEmpty)(self)

  def pluralize(amount: Long, pluralSuffix: String = "s", singularSuffix: String = ""): String =
    s"$self${if (amount.abs == 1) singularSuffix else pluralSuffix}"

  // =====| Alignment |=====

  private def alignFunction(length: Int, char: Char)(padF: Int => (Option[Int], Option[Int])): String = {
    val toAdd = length - self.length
    val charStr = char.toString
    if (toAdd > 0) {
      val (left, right) = padF(toAdd)
      List(left.map(charStr * _), self.some, right.map(charStr * _)).flatten.mkString
    } else self
  }

  def alignLeft(length: Int, char: Char = ' '): String =
    alignFunction(length, char)(toAdd => (None, toAdd.some))

  def alignRight(length: Int, char: Char = ' '): String =
    alignFunction(length, char)(toAdd => (toAdd.some, None))

  def alignCenter(length: Int, char: Char = ' '): String =
    alignFunction(length, char) { toAdd =>
      val left = toAdd / 2
      val right = toAdd - left
      (left.some, right.some)
    }

  // =====| Color |=====

  def stripColor: String =
    self.replaceAll("\u001b\\[\\d+(;\\d+)*m", "")

}
