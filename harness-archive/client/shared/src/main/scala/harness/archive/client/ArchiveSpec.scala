package harness.archive.client

import harness.http.client.*
import zio.*

final case class ArchiveSpec (appName: String, baseUrl: String, httpClient: HttpClient.ClientT)
object ArchiveSpec {
  
  def layer(appName: String, baseUrl: String): URLayer[HttpClient.ClientT, ArchiveSpec] =
    ZLayer.fromZIO { ZIO.serviceWith[HttpClient.ClientT](ArchiveSpec(appName, baseUrl, _)) }
  
}
