package harness.zio

import zio.*
import harness.core.*

trait FileSystemCompanionPlatformSpecific {
  val liveLayer: Layer[HError, FileSystem]
}
