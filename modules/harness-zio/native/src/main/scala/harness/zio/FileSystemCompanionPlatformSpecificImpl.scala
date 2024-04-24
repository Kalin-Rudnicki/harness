package harness.zio

import zio.*

trait FileSystemCompanionPlatformSpecificImpl { self: FileSystemCompanionPlatformSpecific =>

  // TODO (KR) :
  override val liveLayer: TaskLayer[FileSystem] =
    ZLayer.succeed(FileSystem.Unimplemented)

}
