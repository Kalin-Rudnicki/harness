package template.api

import harness.http.server.*
import harness.http.server.test.RouteSpec
import harness.sql.JDBCConnection
import harness.zio.*
import zio.*
import zio.test.*

abstract class CustomRouteSpec extends RouteSpec[Main.ServerEnv, Main.ReqEnv] {

  override final val serverLayer: SHRLayer[Scope, Main.ServerEnv] = Main.serverLayer
  override final val reqLayer: SHRLayer[Main.ServerEnv & JDBCConnection & Scope, Main.ReqEnv] = Main.reqLayer
  override final val route: ServerConfig => Route[Main.ServerEnv & Main.ReqEnv] = Main.routes

  override def aspects: Chunk[TestAspectAtLeastR[Environment]] =
    super.aspects ++ Chunk(TestAspect.samples(15))

}
