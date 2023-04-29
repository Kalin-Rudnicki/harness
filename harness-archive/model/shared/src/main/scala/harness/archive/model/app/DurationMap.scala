package harness.archive.model.app

import harness.zio.*
import harness.zio.ZIOJsonInstances.{fieldEncoderFromStringEncoder, fieldDecoderFromStringDecoder}
import scala.annotation.tailrec
import zio.*
import zio.json.*

final case class DurationMap(map: Map[Logger.LogLevel, Duration], default: Duration) {

  private val fullMap: Map[Logger.LogLevel, Duration] = {
    @tailrec
    def loop(
        queue: List[Logger.LogLevel],
        default: Duration,
        stack: List[(Logger.LogLevel, Duration)],
    ): Map[Logger.LogLevel, Duration] =
      queue match {
        case head :: tail =>
          val duration = map.getOrElse(head, default)
          loop(tail, duration, (head, duration) :: stack)
        case Nil => stack.toMap
      }

    loop(
      Logger.LogLevel.allLevels.sortBy(_.tolerancePriority),
      default,
      Nil,
    )
  }

  def getDuration(logLevel: Logger.LogLevel): Duration = fullMap(logLevel)
  def getDuration(logLevel: Option[Logger.LogLevel]): Duration = logLevel.fold(default)(fullMap(_))

}
object DurationMap {
  
  def make(default: Duration)(pairs: (Logger.LogLevel, Duration)*): DurationMap = DurationMap(pairs.toMap, default)
  
  implicit val jsonCodec: JsonCodec[DurationMap] = DeriveJsonCodec.gen
  
}
