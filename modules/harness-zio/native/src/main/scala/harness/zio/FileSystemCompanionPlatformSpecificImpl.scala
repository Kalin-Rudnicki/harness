package harness.zio

trait FileSystemCompanionPlatformSpecificImpl { self: FileSystemCompanionPlatformSpecific =>

  // TODO (KR) :
  override def defaultFS: FileSystem = UnimplementedFileSystem

}
