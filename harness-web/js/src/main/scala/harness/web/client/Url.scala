package harness.web.client

import harness.zio.*
import org.scalajs.dom.URLSearchParams
import org.scalajs.dom.window
import scala.scalajs.js.URIUtils
import zio.*

final case class Url(path: List[String], params: Map[String, String]) {

  private def show(convert: String => String): String = {
    val pathStr = path.map(convert).mkString("/", "/", "")
    val paramsStr =
      if (params.nonEmpty) s"?${params.toList.sortBy(_._1).map { (k, v) => s"${convert(k)}=${convert(v)}" }.mkString("&")}"
      else ""
    s"$pathStr$paramsStr"
  }

  def showRaw: String = show(identity)
  def showEncoded: String = show(URIUtils.encodeURIComponent)

  override def toString: String = showEncoded

}
object Url {

  def apply(paths: String*)(params: (String, String)*): Url =
    Url(paths.toList, params.toMap)

  val fromWindowURL: HTask[Url] =
    for {
      pathname <- ZIO.hAttempt("Unable to get pathname from window")(window.location.pathname)
      search <- ZIO.hAttempt("Unable to get search from window")(window.location.search)

      path = pathname.split("/").toList.filter(_.nonEmpty)
      params <- ZIO.hAttempt("Unable to create URLSearchParams") {
        new URLSearchParams(search).toList.map { t => (t._1, t._2) }.toMap
      }
    } yield Url(path, params)

}
