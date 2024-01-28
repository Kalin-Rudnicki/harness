package harness.payments

import harness.core.*
import harness.http.client.HttpClient
import harness.payments.facades.*
import harness.payments.model.Currency
import harness.payments.model.ids.*
import harness.webUI.*
import harness.webUI.style.{given, *}
import harness.webUI.vdom.{given, *}
import harness.webUI.widgets.*
import harness.zio.*
import org.scalajs.dom.{console, document, window}
import scala.util.matching.Regex
import zio.*

object PaymentsUI {

  private val loader: Promise[Nothing, Unit] =
    Unsafe.unsafely { Runtime.default.unsafe.run { Promise.make[Nothing, Unit] }.getOrThrow() }

  val addStripeSrc: HRIO[Logger, Unit] =
    Logger.log.debug("Loading stripe src") *>
      ZIO
        .hAttempt {
          val elem = document.createElement("script")
          elem.setAttribute("src", "https://js.stripe.com/v3/")
          elem.asInstanceOf[scalajs.js.Dynamic].onload = { (_: Any) =>
            Unsafe.unsafely { Runtime.default.unsafe.run { loader.done(Exit.Success(())) }.getOrThrow() }
          }
          document.head.append(elem)
        }
        .mapError(HError.SystemFailure("Unable to add stripe src", _))

  val awaitStripeSrc: UIO[Unit] =
    loader.await

  private val elementName: String = "payment-element"

  def createAndMountElements(payments: PaymentEnv, currency: Currency): HTask[Unit] =
    ZIO
      .hAttempt {
        val options = ElementsOptions("setup", currency.toString.toLowerCase)
        val elements = payments.stripe.elements(options)
        val paymentElement = elements.create("payment")
        paymentElement.mount(s"#$elementName")
        elements
      }
      .mapError(HError.SystemFailure("Unable to create & mount stripe payments elements", _))
      .flatMap(payments.elementsRef.set)

  private implicit class MaybeErrorOps(maybeError: MaybeErrorResponse) {
    def toZIO: HTask[Unit] =
      maybeError.error.toOption match {
        case Some(error) => ZIO.fail(HError.UserError(s"[TODO - message]: ${error.message}"))
        case None        => ZIO.unit
      }
  }

  final class PaymentEnv private (
      private[PaymentsUI] val stripe: Stripe,
      private[PaymentsUI] val elementsRef: Ref[Elements],
  ) {

    val elements: UIO[Elements] =
      elementsRef.get.tap { elements => ZIO.dieMessage("elements not populated").when(elements == null) }

  }
  object PaymentEnv {

    def create(apiKey: String): UIO[PaymentEnv] =
      Ref.make[Elements](null).map { new PaymentEnv(Stripe(apiKey), _) }

  }

  private val getUrlRegex: Regex =
    s"^(https?://[^/]+)".r

  def paymentForm(
      createIntent: HRIO[HttpClient.ClientT & Logger & Telemetry, ClientSecret],
      paymentsEnv: PaymentEnv,
      redirectUrl: Url,
  ): CModifier =
    PModifier.builder
      .withAction[Submit] { rh =>
        form(
          id := "payment-form",
          div(
            id := elementName,
          ),
          FormWidgets
            .submitButton(
              "Submit",
            )
            .flatMapActionZM(_ => ZIO.succeed(Nil)), // otherwise we submit twice, consider making normal button?
          onSubmit := { event =>
            event.preventDefault()
            rh.raiseAction(Submit)
          },
        )
      }
      .flatMapActionZM { _ =>
        for {
          elements <- paymentsEnv.elements
          _ <- ZIO.fromPromiseJS { elements.submit() }.mapError(HError.fromThrowable).flatMap(_.toZIO)
          clientSecret <- createIntent
          _ <- Logger.log.info(s"clientSecret: $clientSecret")
          host = s"${window.location.protocol}//${window.location.host}"
          _ <-
            ZIO
              .fromPromiseJS { paymentsEnv.stripe.confirmSetup(new ConfirmSetupOptions(elements, clientSecret.value, new ConfirmParams(s"$host${redirectUrl.toString}"))) }
              .mapError(HError.fromThrowable)
              .flatMap(_.toZIO)
        } yield Nil
      }

}
