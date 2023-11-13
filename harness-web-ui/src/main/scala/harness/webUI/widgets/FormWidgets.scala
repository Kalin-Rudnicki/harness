package harness.webUI.widgets

import cats.syntax.either.*
import cats.syntax.option.*
import harness.core.*
import harness.webUI.*
import harness.webUI.style.{given, *}
import harness.webUI.vdom.{given, *}
import org.scalajs.dom.{console, KeyboardEvent}

object FormWidgets {

  private def genFormInput[V](
      tag: CNodeWidget,
      filterSubmit: KeyboardEvent => Boolean,
  )(implicit decoder: StringDecoder[V]): NodeWidgetAV[Submit, String, Option[V]] =
    tag(
      PModifier.builder.withState[String] { value := _ },
      PModifier.builder.withAction[Submit].withState[String] { (rh, _) =>
        PModifier(
          onKeyUp := { e =>
            val target = e.target.asInstanceOf[scala.scalajs.js.Dynamic]
            val targetValue = target.value.asInstanceOf[String]

            if (filterSubmit(e)) {
              e.preventDefault()
              rh.raise(
                Raise.setState {
                  targetValue
                },
                Raise.Action(Submit),
              )
            } else rh.setState { targetValue }
          },
          onChange := { e =>
            val target = e.target.asInstanceOf[scala.scalajs.js.Dynamic]
            val targetValue = target.value.asInstanceOf[String]

            rh.setState(targetValue)
          },
        )
      },
    ).flatMapValueS { (s, _) =>
      if (s.nonEmpty) decoder.decodeAccumulating(s).map(_.some)
      else None.asRight
    }

  def formInput[V: StringDecoder]: NodeWidgetAV[Submit, String, Option[V]] =
    genFormInput[V](input, e => e.keyCode == KeyCode.Enter.keyCode)

  def formTextArea[V: StringDecoder]: NodeWidgetAV[Submit, String, Option[V]] =
    genFormInput[V](textarea, e => e.keyCode == KeyCode.Enter.keyCode && e.ctrlKey)

  def formSubmitButton: CNodeWidgetA[Submit] =
    button(
      PModifier.builder.withAction[Submit] { rh =>
        onClick := { _ => rh.raise(Raise.Action(Submit)) }
      },
    )

  def labelRequired[A, S, V](
      widget: NodeWidgetAV[A, S, Option[V]],
  )(
      _label: String,
      _id: String,
      inputModifier: CModifier = PModifier(),
      labelModifier: CModifier = PModifier(),
  ): ModifierAV[A, S, V] =
    div(DefaultStyleSheet.formField).defer(
      widget
        .apply(
          DefaultStyleSheet.formField.input,
          id := _id,
          inputModifier,
        )
        .required
        .labeled(
          _label,
          label(
            _,
            DefaultStyleSheet.formField.label,
            `for` := _id,
            labelModifier,
          ),
        ),
    )

  def labelOptional[A, S, V](
      widget: NodeWidgetAV[A, S, Option[V]],
  )(
      _label: String,
      _id: String,
      inputModifier: CModifier = PModifier(),
      labelModifier: CModifier = PModifier(),
  ): ModifierAV[A, S, Option[V]] =
    div(DefaultStyleSheet.formField).defer(
      widget
        .apply(
          DefaultStyleSheet.formField.input,
          id := _id,
          inputModifier,
        )
        .labeled(
          _label,
          label(
            _,
            DefaultStyleSheet.formField.label,
            `for` := _id,
            labelModifier,
          ),
        ),
    )

  object labeled {

    def requiredTextInput[V: StringDecoder](
        _label: String,
        _id: String,
        inputModifier: CModifier = PModifier(),
        labelModifier: CModifier = PModifier(),
    ): ModifierAV[Submit, String, V] =
      labelRequired(formInput[V])(_label, _id, inputModifier, labelModifier)

    def requiredTextArea[V: StringDecoder](
        _label: String,
        _id: String,
        inputModifier: CModifier = PModifier(),
        labelModifier: CModifier = PModifier(),
    ): ModifierAV[Submit, String, V] =
      labelRequired(formTextArea[V])(_label, _id, inputModifier, labelModifier)

    def optionalTextInput[V: StringDecoder](
        _label: String,
        _id: String,
        inputModifier: CModifier = PModifier(),
        labelModifier: CModifier = PModifier(),
    ): ModifierAV[Submit, String, Option[V]] =
      labelOptional(formInput[V])(_label, _id, inputModifier, labelModifier)

    def optionalTextArea[V: StringDecoder](
        _label: String,
        _id: String,
        inputModifier: CModifier = PModifier(),
        labelModifier: CModifier = PModifier(),
    ): ModifierAV[Submit, String, Option[V]] =
      labelOptional(formTextArea[V])(_label, _id, inputModifier, labelModifier)

  }

}
