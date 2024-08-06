package harness.cli.error

import harness.cli.*

sealed trait BuildError extends Throwable {

  override def toString: String = this match
    case BuildError.DuplicateParams(duplicates) => s"Parser contains conflicting params: ${duplicates.toList.sortBy(_.showParam.toRawString).map(_.showParam).mkString(", ")}"

  override def getMessage: String = this.toString

}
object BuildError {
  final case class DuplicateParams(duplicates: Set[SimpleName]) extends BuildError
}
