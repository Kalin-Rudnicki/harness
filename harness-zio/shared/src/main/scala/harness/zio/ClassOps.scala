package harness.zio

extension (self: Class[_]) {
  def getNameWithoutPackage: String =
    self.getName.stripPrefix(s"${self.getPackageName}.")
}
