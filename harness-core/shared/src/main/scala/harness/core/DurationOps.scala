package harness.core

extension (self: java.time.Duration) {

  def prettyPrint: String =
    List[(String, Long)](
      "d" -> self.toDaysPart,
      "h" -> self.toHoursPart,
      "m" -> self.toMinutesPart,
      "s" -> self.toSecondsPart,
      "ms" -> self.toMillisPart,
      "ns" -> (self.toNanosPart % 1000000),
    ).filterNot(_._2 == 0)
      .map { (l, n) => s"$n$l" }
      .mkString(" ")

}
