package harness.zio

trait FileSystemCompanionPlatformSpecific {
  val liveLayer: HTaskLayer[FileSystem]
}
