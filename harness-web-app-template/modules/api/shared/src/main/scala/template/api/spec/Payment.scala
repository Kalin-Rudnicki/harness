package template.api.spec

import harness.endpoint.spec.*
import harness.endpoint.typeclass.*
import harness.endpoint.types.*
import harness.payments.model as PM
import template.api.model.error.ApiError
import template.api.model as A
import zio.Chunk

final case class Payment[F[_ <: EndpointType.Any]](
    createIntent: F[Payment.CreateIntent],
    acceptSetupIntent: F[Payment.AcceptSetupIntent],
    paymentMethods: F[Payment.PaymentMethods],
)
object Payment {

  type CreateIntent = EndpointType[A.user.UserToken, Unit, BodyType.None, BodyType.Encoded[PM.ids.ClientSecret], ApiError]
  type AcceptSetupIntent = EndpointType[(PM.ids.SetupIntentId, A.user.UserToken), PM.ids.SetupIntentId, BodyType.None, BodyType.None, ApiError]
  type PaymentMethods = EndpointType[A.user.UserToken, Unit, BodyType.None, BodyType.Encoded[Chunk[A.paymentMethod.PaymentMethod]], ApiError]

  def spec(authToken: headerOrCookie[A.user.UserToken]): Payment[EndpointSpec] =
    "payment" /:
      Payment[EndpointSpec](
        createIntent = EndpointSpec.post("Create Setup Intent") / "intent" / "create"
          /# authToken /--> body.json[PM.ids.ClientSecret] /!--> errorBody.json[ApiError],
        acceptSetupIntent = EndpointSpec.get("Accept Setup Intent") / "intent" / "accept"
          /? query[PM.ids.SetupIntentId]("setup_intent") /# authToken /!--> errorBody.json[ApiError],
        paymentMethods = EndpointSpec.get("Get All Payment Methods") / "method" / "all"
          /# authToken /--> body.json[Chunk[A.paymentMethod.PaymentMethod]] /!--> errorBody.json[ApiError],
      )

}
