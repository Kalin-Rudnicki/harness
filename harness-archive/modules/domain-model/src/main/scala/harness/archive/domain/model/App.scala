package harness.archive.domain.model

import harness.archive.api.model as Api
import harness.zio.Logger
import zio.*

final case class App(
    id: Api.app.AppId,
    userId: Api.user.UserId,
    name: String,
    logDurationMap: Api.app.DurationMap,
    traceDurationMap: Api.app.DurationMap,
)
object App {

  def initial(user: User, appName: String): App =
    App(
      Api.app.AppId.gen,
      user.id,
      appName,
      Api.app.DurationMap.make(5.minutes)(
        Logger.LogLevel.Trace -> 1.hour,
        Logger.LogLevel.Debug -> 6.hours,
        Logger.LogLevel.Info -> 1.day,
        Logger.LogLevel.Warning -> 2.days,
        Logger.LogLevel.Error -> 7.days,
      ),
      Api.app.DurationMap.make(5.minutes)(
        Logger.LogLevel.Trace -> 6.hours,
        Logger.LogLevel.Debug -> 12.hours,
        Logger.LogLevel.Info -> 1.day,
        Logger.LogLevel.Warning -> 2.days,
        Logger.LogLevel.Error -> 7.days,
      ),
    )

}
