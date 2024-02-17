package harness.zio.error

sealed trait FSError extends Throwable
object FSError {

  // =====| Create |=====

  final case class FailedToCreateSymLink(linkPath: String, targetPath: String, cause: Throwable) extends FSError
  final case class FailedToCreateLink(linkPath: String, existingPath: String, cause: Throwable) extends FSError
  final case class FailedToCreateFile(path: String, cause: Throwable) extends FSError
  final case class FailedToCreateDirectory(path: String, cause: Throwable) extends FSError
  final case class FailedToCreateDirectories(path: String, cause: Throwable) extends FSError

  // =====| Delete |=====

  final case class FailedToDelete(path: String, cause: Throwable) extends FSError

  // =====| Getting a Path |=====

  final case class PathParentDNE(path: String) extends FSError
  final case class FailedToGetParent(path: String, cause: Throwable) extends FSError
  final case class FailedToGetChild(path: String, child: String, cause: Throwable) extends FSError
  final case class FailedToGetChildren(path: String, cause: Throwable) extends FSError

  // =====| Read / Write |=====

  final case class UnableToReadFromFile(path: String, cause: Throwable) extends FSError
  final case class UnableToWriteToFile(path: String, cause: Throwable) extends FSError
  final case class UnableToCreateOutputStream(path: String, cause: Throwable) extends FSError
  final case class UnableToCreateInputStream(path: String, cause: Throwable) extends FSError

  // =====| Validations |=====

  final case class PathDNE(path: String) extends FSError
  final case class PathIsNotAFile(path: String) extends FSError
  final case class PathIsNotADirectory(path: String) extends FSError

  // =====| Other |=====

  final case class UnableToResolvePath(path: String, cause: Throwable) extends FSError
  final case class UnableToGetProperty(path: String, property: String, cause: Throwable) extends FSError
  final case class UnableToSetProperty(path: String, property: String, cause: Throwable) extends FSError
  final case class UnableToGetHomeDirectory(cause: Throwable) extends FSError
  final case class UnableToGetRoots(cause: Throwable) extends FSError

  // =====| General |=====

  final case class MalformedFileContents(message: String, path: String) extends FSError

  final case class Generic(message: String, path: String, cause: Throwable) extends FSError

}
