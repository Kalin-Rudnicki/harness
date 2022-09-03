package harness.zio

import harness.core.*
import zio.*

trait FileSystemCompanionPlatformSpecificImpl { self: FileSystemCompanionPlatformSpecific =>
  override val liveLayer: Layer[HError, FileSystem] = ZLayer.succeed(FileSystem.Unimplemented)
}
