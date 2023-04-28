package harness.core

import cats.syntax.option.*
import scala.util.matching.Regex

private val camelRegex: Regex = "\\d+|[A-Z](?=[a-z])|[A-Z]+(?:(?=$)|(?![^A-Z]))".r
private val snakeRegex: Regex = "_[a-zA-Z0-9]".r

extension (self: String) {

  // =====| Misc |=====

  def toNES: Option[String] =
    Option.when(self.nonEmpty)(self)

  def pluralize(amount: Long, pluralSuffix: String = "s", singularSuffix: String = ""): String =
    s"$self${if (amount.abs == 1) singularSuffix else pluralSuffix}"

  def decapitalize: String =
    if (self == null || self.length == 0 || !self.charAt(0).isUpper) self
    else self.updated(0, self.charAt(0).toLower)

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

  // =====| Casing |=====

  def camelToSnake: String =
    camelRegex.replaceAllIn(self, m => if (m.start == 0) m.matched else s"_${m.matched}").toLowerCase

  def snakeToLowerCamel: String =
    snakeRegex.replaceAllIn(self, _.matched.substring(1).capitalize)

  def snakeToUpperCamel: String =
    self.snakeToLowerCamel.capitalize

  def dashToSnake: String =
    self.replace('-', '_')

  def snakeToDash: String =
    self.replace('_', '-')

}

implicit class HarnessStrOps(self: String) {

  def unesc: String = self.unesc("\"")
  def unesc(leftAndRight: String): String = self.unesc(leftAndRight, leftAndRight)
  def unesc(left: String, right: String): String =
    s"$left${self.toList.map(_.unesc(""))}$right"
  
}
