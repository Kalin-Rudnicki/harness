package harness.archive.client

import harness.zio.*

implicit class ExecutableAppConfigOps(config: ExecutableApp.Config) {

  def addArchiveDecoders: ExecutableApp.Config =
    config
      .addLoggerDecoders(ArchiveLoggerTarget.keyedConfigDecoder)
      .addTelemetryDecoders(ArchiveTelemetry.keyedConfigDecoder)

}
