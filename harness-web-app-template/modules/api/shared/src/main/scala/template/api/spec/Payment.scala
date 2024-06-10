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
    acceptIntent: F[Payment.AcceptIntent],
    paymentMethods: F[Payment.PaymentMethods],
)
object Payment {

  type CreateIntent = EndpointType.Builder#Auth[A.user.UserToken]#OutputBodyEncoded[PM.ids.ClientSecret]#Error[ApiError]#Build
  type AcceptIntent = EndpointType.Builder#Query[PM.ids.SetupIntentId]#Auth[A.user.UserToken]#Error[ApiError]#Build
  type PaymentMethods = EndpointType.Builder#Auth[A.user.UserToken]#OutputBodyEncoded[Chunk[A.paymentMethod.PaymentMethod]]#Error[ApiError]#Build

  def spec(authToken: headerOrCookie[A.user.UserToken]): Payment[EndpointSpec] =
    "payment" /:
      Payment[EndpointSpec](
        createIntent = EndpointSpec.post("Create Setup Intent") / "intent" / "create"
          /!# authToken /--> body.json[PM.ids.ClientSecret] /!--> errorBody.json[ApiError],
        acceptIntent = EndpointSpec.get("Accept Setup Intent") / "intent" / "accept"
          /? query[PM.ids.SetupIntentId]("setup_intent") /!# authToken /!--> errorBody.json[ApiError],
        paymentMethods = EndpointSpec.get("Get All Payment Methods") / "method" / "all"
          /!# authToken /--> body.json[Chunk[A.paymentMethod.PaymentMethod]] /!--> errorBody.json[ApiError],
      )

}
