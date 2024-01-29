package harness.archive.api

import harness.archive.api.main.ServerMain
import harness.http.server.*
import harness.http.server.test.RouteSpec
import harness.sql.JDBCConnection
import harness.zio.*
import zio.*
import zio.test.*

abstract class CustomRouteSpec extends RouteSpec[ServerMain.ServerEnv, ServerMain.ReqEnv] {

  override final val serverLayer: SHRLayer[Scope, ServerMain.ServerEnv] = ServerMain.serverLayer
  override final val reqLayer: SHRLayer[ServerMain.ServerEnv & JDBCConnection & Scope, ServerMain.ReqEnv] = ServerMain.reqLayer
  override final val route: URIO[HarnessEnv & ServerEnv & ServerConfig, Route[ServerMain.ServerEnv & ServerMain.ReqEnv]] = ServerMain.routes

  override def aspects: Chunk[TestAspectAtLeastR[Environment]] =
    super.aspects ++ Chunk(TestAspect.samples(15))

}
