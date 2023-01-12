package harness.web.client

import harness.web.client.vdom.*
import harness.zio.*
import java.util.UUID

final case class PageMessage(
    id: UUID,
    title: String,
    logLevel: Logger.LogLevel,
    style: CModifier,
) {
  def withStyle(style: CModifier): PageMessage = this.copy(style = style)
  def appendStyle(style: CModifier): PageMessage = this.copy(style = PModifier(this.style, style))
}
object PageMessage {

  private inline def randomUUID: UUID =
    new UUID(scala.util.Random.nextLong(), scala.util.Random.nextLong())

  def info(title: String): PageMessage =
    PageMessage(randomUUID, title, Logger.LogLevel.Info, backgroundColor.green)

  def error(title: String): PageMessage =
    PageMessage(randomUUID, title, Logger.LogLevel.Error, backgroundColor.red)

}
