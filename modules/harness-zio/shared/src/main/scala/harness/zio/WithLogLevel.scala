package harness.zio

class WithLogLevel[A](make: Logger.LogLevel => A) {

  def apply(level: Logger.LogLevel): A = make(level)

  def never: A = make(Logger.LogLevel.Never)
  def trace: A = make(Logger.LogLevel.Trace)
  def debug: A = make(Logger.LogLevel.Debug)
  def detailed: A = make(Logger.LogLevel.Detailed)
  def info: A = make(Logger.LogLevel.Info)
  def important: A = make(Logger.LogLevel.Important)
  def warning: A = make(Logger.LogLevel.Warning)
  def error: A = make(Logger.LogLevel.Error)
  def fatal: A = make(Logger.LogLevel.Fatal)
  def always: A = make(Logger.LogLevel.Always)

}
object WithLogLevel {

  def make[A](f: Logger.LogLevel => A): WithLogLevel[A] = new WithLogLevel[A](f)

}
