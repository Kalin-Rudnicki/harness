package harness.archive.webServer.api

import harness.archive.api.model as Api
import harness.archive.domain.model.*
import harness.archive.domain.storage.*
import harness.zio.*
import zio.*

final case class AppApi(
    sessionStorage: SessionStorage,
    appStorage: AppStorage,
    appTokenStorage: AppTokenStorage,
) {

  def getAll(token: Api.user.UserToken): ZIO[HarnessEnv, DomainError, Chunk[App]] =
    for {
      user <- SessionUtils.userFromSessionToken(token, sessionStorage)
      apps <- appStorage.selectAll(user.id)
    } yield apps

  def create(token: Api.user.UserToken, appName: String): ZIO[HarnessEnv, DomainError, App] =
    for {
      user <- SessionUtils.userFromSessionToken(token, sessionStorage)
      existingApp <- appStorage.byName(user.id, appName)
      _ <- ZIO.fail(DomainError.AppNameAlreadyExists(user.id, appName)).when(existingApp.nonEmpty)
      app = App.initial(user, appName)
      _ <- appStorage.insert(app)
    } yield app

  def generateApiToken(token: Api.user.UserToken, appId: Api.app.AppId, tokenName: String): ZIO[HarnessEnv, DomainError, Api.app.AppToken] =
    for {
      now <- Clock.currentDateTime
      user <- SessionUtils.userFromSessionToken(token, sessionStorage)
      app <- appStorage.byId(appId)
      _ <- ZIO.fail(DomainError.UserDoesNotHaveAccessToApp(user.id, app.id)).unless(app.userId == user.id)
      appToken = AppToken.forApp(app, tokenName, now)
      _ <- appTokenStorage.insert(appToken)
    } yield appToken.token

}
object AppApi {

  val layer: URLayer[SessionStorage & AppStorage & AppTokenStorage, AppApi] =
    ZLayer.fromFunction { AppApi.apply }

}
