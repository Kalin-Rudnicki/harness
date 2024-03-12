package harness.archive.webServer.api

import harness.archive.api.model as Api
import harness.archive.domain.model.*
import harness.archive.domain.storage.*
import harness.core.*
import harness.zio.*
import zio.*

final case class LogApi(
    sessionStorage: SessionStorage,
    appStorage: AppStorage,
    appTokenStorage: AppTokenStorage,
    logStorage: LogStorage,
) {

  def uploadLog(appToken: Api.app.AppToken, upload: Api.log.Upload): ZIO[HarnessEnv, DomainError, Unit] =
    for {
      appToken <- SessionUtils.appTokenFromToken(appToken, appTokenStorage)
      app <- appStorage.byId(appToken.appId)
      _ <- Logger.log.info(s"Received request to create ${upload.logs.length.pluralizeOn("log")} for app ${appToken.appId}")
      logs = upload.logs.map { Log.create(app, _) }
      _ <- logStorage.insertAll(logs)
    } yield ()

  def getForApp(token: Api.user.UserToken, appName: String): ZIO[HarnessEnv, DomainError, Chunk[Log]] =
    for {
      user <- SessionUtils.userFromSessionToken(token, sessionStorage)
      app <- appStorage.byName(user.id, appName).someOrFail(DomainError.AppNotFound(user.id, appName))
      logs <- logStorage.forAppId(app.id)
    } yield logs

  def get(token: Api.user.UserToken, query: String): ZIO[HarnessEnv, DomainError, Chunk[Log]] =
    ??? // TODO (KR) :

}
object LogApi {

  val layer: URLayer[SessionStorage & AppStorage & AppTokenStorage & LogStorage, LogApi] =
    ZLayer.fromFunction { LogApi.apply }

}
