package harness.zio

import harness.core.*
import zio.*

trait FileSystemCompanionPlatformSpecific {
  val liveLayer: HTaskLayer[FileSystem]
}
