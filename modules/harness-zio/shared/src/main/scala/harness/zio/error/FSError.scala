package harness.zio.error

import harness.core.*

sealed trait FSError extends Throwable {

  override final def getMessage: String = this match {
    case FSError.FailedToCreateSymLink(linkPath, targetPath, cause) =>
      s"Failed to create sym-link with link='$linkPath' target='$targetPath': ${cause.safeGetMessage}"
    case FSError.FailedToCreateLink(linkPath, existingPath, cause) =>
      s"Failed to create sym-link with link='$linkPath' existing='$existingPath': ${cause.safeGetMessage}"
    case FSError.FailedToCreateFile(path, cause) =>
      s"Failed to create file with path='$path': ${cause.safeGetMessage}"
    case FSError.FailedToCreateDirectory(path, cause) =>
      s"Failed to create directory with path='$path': ${cause.safeGetMessage}"
    case FSError.FailedToCreateDirectories(path, cause) =>
      s"Failed to create directories with path='$path': ${cause.safeGetMessage}"
    case FSError.FailedToDelete(path, cause) =>
      s"Failed to delete file with path='$path': ${cause.safeGetMessage}"
    case FSError.PathParentDNE(path) =>
      s"Path '$path' parent does not exist"
    case FSError.FailedToGetParent(path, cause) =>
      s"Error getting parent of path '$path': ${cause.safeGetMessage}"
    case FSError.FailedToGetChild(path, child, cause) =>
      s"Error resolving child path with path='$path' child='$child': ${cause.safeGetMessage}"
    case FSError.FailedToGetChildren(path, cause) =>
      s"Error getting children of path '$path': ${cause.safeGetMessage}"
    case FSError.UnableToReadFromFile(path, cause) =>
      s"Error reading from path '$path': ${cause.safeGetMessage}"
    case FSError.UnableToWriteToFile(path, cause) =>
      s"Error writing to path '$path': ${cause.safeGetMessage}"
    case FSError.UnableToCreateOutputStream(path, cause) =>
      s"Error creating output stream for path '$path': ${cause.safeGetMessage}"
    case FSError.UnableToCreateInputStream(path, cause) =>
      s"Error creating input stream for path '$path': ${cause.safeGetMessage}"
    case FSError.PathDNE(path) =>
      s"Path '$path' does not exist"
    case FSError.PathIsNotAFile(path) =>
      s"Path '$path' is not a file"
    case FSError.PathIsNotADirectory(path) =>
      s"Path '$path' is not a directory"
    case FSError.UnableToResolvePath(path, cause) =>
      s"Unable to resolve path '$path': ${cause.safeGetMessage}"
    case FSError.UnableToGetProperty(path, property, cause) =>
      s"Error getting property '$property' of path '$path': ${cause.safeGetMessage}"
    case FSError.UnableToSetProperty(path, property, cause) =>
      s"Error setting property '$property' of path '$path': ${cause.safeGetMessage}"
    case FSError.UnableToGetHomeDirectory(cause) =>
      s"Unable to get home directory: ${cause.safeGetMessage}"
    case FSError.UnableToGetRoots(cause) =>
      s"Unable to get file-system roots: ${cause.safeGetMessage}"
    case FSError.UnableToDecodeFileContents(message, path) =>
      s"Unable to decode file contents of path '$path': $message"
    case FSError.Generic(message, path, cause) =>
      s"""Accessing file-system encountered generic error:
         |  path: $path
         |  message: $message
         |  cause: ${cause.safeGetMessage}""".stripMargin
  }

}
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

  final case class UnableToDecodeFileContents(message: String, path: String) extends FSError

  final case class Generic(message: String, path: String, cause: Throwable) extends FSError

}
