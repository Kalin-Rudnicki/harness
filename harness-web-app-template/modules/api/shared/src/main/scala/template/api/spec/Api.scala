package template.api.spec

import harness.endpoint.spec.*
import harness.endpoint.types.*
import template.api.model as A

final case class Api[F[_ <: EndpointType.Any]](
    user: User[F],
    payment: Payment[F],
)
object Api {

  def spec(authToken: headerOrCookie[A.user.UserToken]): Api[EndpointSpec] =
    Api(
      user = User.spec(authToken),
      payment = Payment.spec(authToken),
    )

}
