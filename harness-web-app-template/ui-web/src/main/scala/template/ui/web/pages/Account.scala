package template.ui.web.pages

import _root_.template.model as D
import _root_.template.ui.web.helpers.*
import cats.syntax.either.*
import cats.syntax.option.*
import harness.core.*
import harness.payments.PaymentsUI
import harness.payments.facades.*
import harness.payments.model.Currency
import harness.payments.model.result.TypeDetails
import harness.webUI.*
import harness.webUI.style.{given, *}
import harness.webUI.vdom.{given, *}
import harness.webUI.widgets.*
import harness.zio.*
import zio.*

object Account {

  final case class Env(
      user: D.user.User,
      paymentMethods: Chunk[D.paymentMethod.PaymentMethod],
      createPaymentEnv: PaymentsUI.PaymentEnv,
      createPayment: Boolean,
  )

  val page: Page =
    Page.builder
      .fetchState {
        for {
          user <- Api.user.fromSessionTokenOrRedirectToLogin
          paymentMethods <- Api.payment.paymentMethods
          createPaymentEnv <-
            PaymentsUI.awaitStripeSrc *>
              PaymentsUI.PaymentEnv.create("pk_test_51OVNiaD9gOL4yaWVMKdhPEOW59IeVbst1031HrqDDQRswYNFYAQtiOg9UDSyST7DYLGq8CYVN0bG0q51GovrVpVz0070Gb4ccu")
        } yield Env(
          user,
          paymentMethods,
          createPaymentEnv,
          false,
        )
      }
      .postLoad { state =>
        Logger.log.debug("page loaded!") *>
          PaymentsUI.createAndMountElements(state.createPaymentEnv, Currency.USD)
      }
      .constTitle("Account")
      .body {
        PModifier(
          Widgets.signedInNavBar.zoomOut[Env](_.user).zoomOutToPage,
          PageWidgets.pageBody(
            h1("Account"),
            PModifier.builder.withRaise.withState[Env] { (rh, env) =>
              PModifier(
                div(
                  p(s"Username: ${env.user.username}"),
                  p(s"Name: ${env.user.firstName} ${env.user.lastName}"),
                  p(s"Email: ${env.user.email}"),
                  p("Payment Methods:"),
                  table(
                    DefaultStyleSheet.stdTable,
                    tr(
                      th(width := 200.px, "Details"),
                    ),
                    PModifier.foreach(env.paymentMethods) { pm =>
                      tr(
                        td(
                          pm.typeDetails.map { case TypeDetails.Card(brand, expMonth, expYear, last4) =>
                            PModifier(s"$brand *$last4 (${expMonth.toString.alignRight(2, '0')}/$expYear)")
                          },
                        ),
                      )
                    },
                  ),
                ),
                br,
                div(
                  Option.when(!env.createPayment) {
                    button(
                      DefaultStyleSheet.button.primary,
                      "Add Payment Method",
                      onClick := { _ => rh.updateState(_.copy(createPayment = true)) },
                    )
                  },
                ),
                div(
                  if (env.createPayment) display.block else display.none,
                  PaymentsUI.paymentForm(Api.payment.createIntent, env.createPaymentEnv, Url("api", "payment", "accept-setup-intent")()),
                ),
              )
            },
          ),
        )
      }
      .logA

}
