package template.api.impl

import harness.http.server.Implementation
import harness.sql.query.Transaction
import template.api.service.*
import template.api.spec as Spec
import template.domain.model.DomainError
import template.domain.session.SessionService

object Api {

  type Env = UserApi & PaymentApi & SessionService & Transaction[DomainError]

  val impl: Spec.Api[Implementation.Projection[Env]] =
    Spec.Api[Implementation.Projection[Env]](
      user = User.impl,
      payment = Payment.impl,
    )

}
