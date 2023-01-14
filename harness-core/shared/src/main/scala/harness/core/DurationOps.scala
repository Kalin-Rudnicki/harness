package harness.core

extension (self: java.time.Duration) {

  def prettyPrint: String = {
    val millis = self.toMillis
    if (millis < 1000) s"$millis.${((self.toNanos % 1000000) / 1000).toString.alignRight(3, '0')}ms"
    else s"${(millis / 1000).toStringCommas}.${(millis % 1000).toString.alignRight(3, '0')}s"
  }

}
