package harness.archive.api

import harness.archive.api.main.ServerMain
import harness.http.server.*
import harness.http.server.test.RouteSpec
import harness.zio.*
import zio.*
import zio.test.*

abstract class CustomRouteSpec extends RouteSpec[ServerMain.ServerEnv, ServerMain.ReqEnv, Any] {

  override final val serverLayer: SHRLayer[Scope, ServerMain.ServerEnv] = ServerMain.serverLayer
  override final val reqLayer: SHRLayer[ServerMain.ServerEnv & Scope, ServerMain.ReqEnv] = ServerMain.reqLayer
  override final val reqLayerNoConnection: SHRLayer[Any, ReqEnv_NoConnection] = ZLayer.empty
  override final val route: ServerConfig => Route[ServerMain.ServerEnv & ServerMain.ReqEnv] = ServerMain.routes

  override def aspects: Chunk[TestAspectAtLeastR[Environment]] =
    super.aspects ++ Chunk(TestAspect.samples(15))

}
