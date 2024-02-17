package harness.cli

sealed trait Param {
  def formattedName: String // TODO (KR) : ColorString
}
object Param {

  // =====| Types |=====

  final case class ShortFlag(name: ShortName) extends Param {
    override def formattedName: String = Param.formattedShort(name.name.toString, false)
  }

  final case class LongFlag(name: LongName) extends Param {
    override def formattedName: String = Param.formattedLong(name.name, false)
  }

  final case class ShortWithValue(name: ShortName) extends Param {
    override def formattedName: String = Param.formattedShort(name.name.toString, true)
  }

  final case class LongWithValue(name: LongName) extends Param {
    override def formattedName: String = Param.formattedLong(name.name, true)
  }

  final case class ShortToggle(trueName: ShortName, falseName: ShortName) extends Param {
    override def formattedName: String = Param.formattedShort(s"($trueName/$falseName)", false)
  }

  sealed abstract class LongToggle(
      final val prefix: String,
      final val trueName: LongName,
      final val falseName: LongName,
      final val baseName: LongName,
  ) extends Param {

    override def formattedName: String = Param.formattedLong(s"$prefix$baseName", false)

    final def formatShortToggle(shortToggle: ShortToggle): String =
      this match {
        case _: LongToggle.PrefixTrue  => formattedShort(s"[${shortToggle.trueName}]${shortToggle.falseName}", false)
        case _: LongToggle.PrefixFalse => formattedShort(s"[${shortToggle.falseName}]${shortToggle.trueName}", false)
        case _: LongToggle.PrefixBoth  => formattedShort(s"(${shortToggle.trueName}/${shortToggle.falseName})", false)
      }

  }
  object LongToggle {
    final class PrefixTrue(truePrefix: LongName, baseName: LongName)
        extends LongToggle(
          s"[$truePrefix-]",
          truePrefix - baseName,
          baseName,
          baseName,
        )
    final class PrefixFalse(falsePrefix: LongName, baseName: LongName)
        extends LongToggle(
          s"[$falsePrefix-]",
          baseName,
          falsePrefix - baseName,
          baseName,
        )
    final class PrefixBoth(truePrefix: LongName, falsePrefix: LongName, baseName: LongName)
        extends LongToggle(
          s"($truePrefix/$falsePrefix)-",
          truePrefix - baseName,
          falsePrefix - baseName,
          baseName,
        )
  }

  final case class Value(name: LongName, many: Boolean) extends Param {
    override def formattedName: String = s"$name${if (many) "..." else ""}"
  }

  // =====| Helpers |=====

  private def short(name: String): String = s"-$name"
  private def long(name: String): String = s"--$name"

  private def formattedShort(name: String, hasValue: Boolean): String = // TODO (KR) : ColorString
    if (hasValue) s"${short(name)}=VALUE"
    else short(name)
  private def formattedLong(name: String, hasValue: Boolean): String = // TODO (KR) : ColorString
    if (hasValue) s"${long(name)}=VALUE"
    else long(name)

}
