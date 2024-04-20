package harness.endpoint.error

import harness.endpoint.spec.*
import harness.web.HttpCode
import zio.json.*

@jsonDiscriminator("type")
sealed trait ApiInternalDefect
object ApiInternalDefect {

  @errorCode(HttpCode.`500`)
  @errorExamples(InternalDefect)
  case object InternalDefect extends ApiInternalDefect

  implicit val errorSchema: ErrorSchema[ApiInternalDefect] = ErrorSchema.derive

}
