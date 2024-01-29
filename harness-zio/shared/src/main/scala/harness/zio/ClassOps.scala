package harness.zio

extension (self: Class[?]) {
  def getNameWithoutPackage: String =
    self.getName.stripPrefix(s"${self.getPackageName}.")
}
