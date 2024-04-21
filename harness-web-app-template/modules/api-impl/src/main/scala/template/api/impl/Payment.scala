package template.api.impl

import harness.http.server.*
import template.api.service.*
import template.api.spec as Spec
import zio.*

object Payment {

  val impl: Spec.Payment[Implementation.Projection[Api.Env]] =
    Spec.Payment[Implementation.Projection[Api.Env]](
      createIntent = Implementation[Spec.Payment.CreateIntent].implement { token =>
        ZIO.serviceWithZIO[PaymentApi](_.createIntent(token)).toHttpResponse
      },
      acceptIntent = Implementation[Spec.Payment.AcceptIntent].implement { (intentId, token) =>
        ZIO.serviceWithZIO[PaymentApi](_.acceptIntent(token, intentId)).toHttpResponse
      },
      paymentMethods = Implementation[Spec.Payment.PaymentMethods].implement { token =>
        ZIO.serviceWithZIO[PaymentApi](_.paymentMethods(token)).map(_.map(_.toApi)).toHttpResponse
      },
    )

}
