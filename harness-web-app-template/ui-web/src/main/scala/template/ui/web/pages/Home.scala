package template.ui.web.pages

import _root_.template.model as D
import _root_.template.ui.web.helpers.*
import cats.syntax.either.*
import cats.syntax.option.*
import harness.core.*
import harness.payments.PaymentsUI
import harness.payments.facades.*
import harness.payments.model.Currency
import harness.webUI.*
import harness.webUI.style.{given, *}
import harness.webUI.vdom.{given, *}
import harness.webUI.widgets.*
import harness.zio.*
import zio.*

object Home {

  final case class Env(
      user: D.user.User,
      requiredTextInput: String,
      optionalTextInput: String,
      requiredTextArea: String,
      optionalTextArea: String,
      requiredDropdown: FormWidgets.dropdownSelect.Env[MyEnum],
      optionalDropdown: FormWidgets.dropdownSelect.Env[MyEnum],
      sumOption: Option[String],
  )

  enum MyEnum extends Enum[MyEnum] { case Case1, Case2, Case3 }
  object MyEnum extends Enum.Companion[MyEnum]

  def show(any: Any): String =
    any.asInstanceOf[Matchable] match {
      case str: String => str.unesc
      case Some(any)   => s"Some(${show(any)})"
      case any         => any.toString
    }

  extension [A, S, V](widget: ModifierAV[A, S, V]) {

    def showValue: ModifierA[A, S] =
      div(
        br,
        div(
          border := "1px solid magenta",
          width := "min-content",
          widget,
        ),
        PModifier.builder.withState[S] { s =>
          div(
            div(
              div(color.cyan, "state:"),
              div(
                paddingLeft := "25px",
                show(s),
              ),
            ),
            widget.value(s) match {
              case Right(value) =>
                div(
                  div(color.green, "value: "),
                  div(whiteSpace.pre, paddingLeft := "25px", show(value)),
                )
              case Left(errors) =>
                div(
                  div(color.red, "errors:"),
                  ul(
                    paddingLeft := "25px",
                    PModifier.foreach(errors.toList)(e => li(e)),
                  ),
                )
            },
          )
        },
      )

  }

  enum SumOptionAction {
    case Show
    case Hide
    case Log(message: String)
  }

  val sumSomeWidget: ModifierA[SumOptionAction, String] =
    PModifier.builder.withAction[SumOptionAction].withState[String] { (rh, _) =>
      (FormWidgets.textInput[String].labelRequired("Message:", "message") <*
        FormWidgets.submitButton("Send Message") <*
        button(
          DefaultStyleSheet.button.primary,
          "Hide",
          onClick := { _ => rh.raiseAction(SumOptionAction.Hide) },
        ))
        .mapActionV { (_, message) => SumOptionAction.Log(message) }
    }

  val sumNoneWidget: CModifierA[SumOptionAction] =
    PModifier.builder.withAction[SumOptionAction] { rh =>
      button(
        DefaultStyleSheet.button.primary,
        "Show",
        onClick := { _ => rh.raiseAction(SumOptionAction.Show) },
      )
    }

  val sumWidget: Modifier[Option[String]] =
    SumWidgets
      .option(sumSomeWidget, sumNoneWidget)
      .flatMapActionZM {
        case SumOptionAction.Show => ZIO.succeed(Raise.setState("".some) :: Nil)
        case SumOptionAction.Hide => ZIO.succeed(Raise.setState(None) :: Nil)
        case SumOptionAction.Log(message) =>
          ZIO.succeed(
            List(
              Raise.DisplayMessage(PageMessage.info(message)),
              Raise.setState(None),
            ),
          )
      }
      .unit

  val page: Page =
    Page.builder
      .fetchState {
        for {
          user <- Api.user.fromSessionTokenOrRedirectToLogin
          _ <- PaymentsUI.awaitStripeSrc
        } yield Env(
          user,
          "",
          "",
          "",
          "",
          FormWidgets.dropdownSelect.Env.enumInitial[MyEnum],
          FormWidgets.dropdownSelect.Env.enumInitial[MyEnum],
          "test".some,
        )
      }
      .constTitle("Home")
      .body {
        PModifier(
          Widgets.signedInNavBar.zoomOut[Env](_.user).zoomOutToPage,
          PageWidgets.pageBody(
            h1("Home"),
            PModifier.builder.withState[D.user.User] { u => p(s"Welcome, ${u.firstName}") }.zoomOut[Env](_.user),
            br,
            h2("Example Widgets:"),
            br,
            button(
              DefaultStyleSheet.button.primary,
              "Show Page Message",
              PModifier.builder.withRaise { rh =>
                onClick := { _ => rh.raise(Raise.DisplayMessage(PageMessage.info("Message"))) }
              },
            ),
            br,
            br,
            FormWidgets.textInput[Int].labelRequired("Required Text Input (Int):", "required-text-input").showValue.zoomOut[Env](_.requiredTextInput),
            FormWidgets.textInput[Int].labelOptional("Optional Text Input (Int):", "optional-text-input").showValue.zoomOut[Env](_.optionalTextInput),
            FormWidgets.textArea[String].labelRequired("Required Text Area (String):", "required-text-area").showValue.zoomOut[Env](_.requiredTextArea),
            FormWidgets.textArea[String].labelOptional("Optional Text Area (String):", "optional-text-area").showValue.zoomOut[Env](_.optionalTextArea),
            FormWidgets.dropdownSelect[MyEnum]().labelRequired("Required Dropdown:", "required-dropdown").showValue.zoomOut[Env](_.requiredDropdown),
            FormWidgets.dropdownSelect[MyEnum](closeOnMouseLeave = true).decorate(width := "150px").labelOptional("Optional Dropdown:", "optional-dropdown").showValue.zoomOut[Env](_.optionalDropdown),
            sumWidget.showValue.zoomOut[Env](_.sumOption),
          ),
        )
      }
      .logA

}
