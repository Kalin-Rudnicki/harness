package harness.zio

trait WithLogLevelAbstract[A] {

  protected val make: Logger.LogLevel => A

  final def apply(level: Logger.LogLevel): A = make(level)

  final def never: A = make(Logger.LogLevel.Never)
  final def trace: A = make(Logger.LogLevel.Trace)
  final def debug: A = make(Logger.LogLevel.Debug)
  final def detailed: A = make(Logger.LogLevel.Detailed)
  final def info: A = make(Logger.LogLevel.Info)
  final def important: A = make(Logger.LogLevel.Important)
  final def warning: A = make(Logger.LogLevel.Warning)
  final def error: A = make(Logger.LogLevel.Error)
  final def fatal: A = make(Logger.LogLevel.Fatal)
  final def always: A = make(Logger.LogLevel.Always)

}

trait WithLogLevel[A](override protected val make: Logger.LogLevel => A) extends WithLogLevelAbstract[A]
object WithLogLevel {
  def make[A](f: Logger.LogLevel => A): WithLogLevel[A] = new WithLogLevel[A](f) {}
}
