package harness.zio

import zio.*

trait FileSystemCompanionPlatformSpecific {
  val liveLayer: TaskLayer[FileSystem]
}
