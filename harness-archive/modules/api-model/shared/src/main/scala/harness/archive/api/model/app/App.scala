package harness.archive.api.model.app

import zio.json.*

final case class App(
    id: AppId,
    name: String,
    logDurationMap: DurationMap,
    traceDurationMap: DurationMap,
)
object App {
  implicit val jsonCodec: JsonCodec[App] = DeriveJsonCodec.gen
}
