package harness.zio

import harness.core.*
import zio.*

trait FileSystemCompanionPlatformSpecificImpl { self: FileSystemCompanionPlatformSpecific =>
  override val liveLayer: HTaskLayer[ FileSystem] = ZLayer.succeed(FileSystem.Unimplemented)
}
