package harness.core

implicit class ThrowableOps(self: Throwable) {
  def safeGetMessage: String = Option(self.getMessage).getOrElse(self.toString)
}
