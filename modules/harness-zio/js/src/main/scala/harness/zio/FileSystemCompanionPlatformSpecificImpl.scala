package harness.zio

import zio.*

trait FileSystemCompanionPlatformSpecificImpl { self: FileSystemCompanionPlatformSpecific =>
  override val liveLayer: TaskLayer[FileSystem] = ZLayer.succeed(FileSystem.Unimplemented)
}
