package harness.web.test

import harness.web.server.{Cookie, HttpRequest}
import zio.*

final case class CookieStorage(cookies: Ref[Map[String, String]])
object CookieStorage {

  def applyCookies(request: HttpRequest): URIO[CookieStorage, HttpRequest] =
    ZIO.serviceWithZIO[CookieStorage] {
      _.cookies.get.map { cookies =>
        request.copy(cookies = cookies ++ request.cookies)
      }
    }

  def update(cookies: List[Cookie]): URIO[CookieStorage, Unit] =
    ZIO.serviceWithZIO[CookieStorage] { cookieStorage =>
      ZIO
        .unless(cookies.isEmpty) {
          cookieStorage.cookies.update { map =>
            val (unset, set) = cookies.partition(_.isUnset)
            map.removedAll(unset.map(_.name)) ++ set.map(c => (c.name, c.value)).toMap
          }
        }
        .unit
    }

  val emptyLayer: ULayer[CookieStorage] =
    ZLayer.fromZIO { Ref.make(Map.empty[String, String]).map(CookieStorage.apply) }

}
