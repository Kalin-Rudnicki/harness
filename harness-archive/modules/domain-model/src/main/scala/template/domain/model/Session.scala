package template.domain.model

import java.util.UUID
import template.api.model as Api

final case class Session(
    id: Api.user.SessionId,
    userId: Api.user.UserId,
    token: Api.user.UserToken,
)
object Session {

  def forUser(user: User): Session =
    Session(Api.user.SessionId.gen, user.id, Api.user.UserToken(s"${UUID.randomUUID()}:${UUID.randomUUID()}"))

}
