package harness.webUI.widgets

import cats.syntax.either.*
import cats.syntax.option.*
import harness.core.*
import harness.webUI.*
import harness.webUI.style.{given, *}
import harness.webUI.vdom.{given, *}
import org.scalajs.dom.{console, KeyboardEvent}

object FormWidgets {

  final case class FormWidget[A, S, V](
      widget: NodeWidgetAV[A, S, Option[V]],
      groupStyle: Option[StyleSheet.StyleSheetPart],
      inputStyle: Option[StyleSheet.StyleSheetPart],
      labelStyle: Option[StyleSheet.StyleSheetPart],
  ) {

    private def labeledGeneric[V2](
        map: NodeWidgetAV[A, S, Option[V]] => NodeWidgetAV[A, S, V2],
    )(
        _label: String,
        _id: String,
        inputModifier: CModifier = PModifier(),
        labelModifier: CModifier = PModifier(),
    ): ModifierAV[A, S, V2] =
      div(groupStyle).defer(
        map(
          widget
            .apply(
              inputStyle,
              id := _id,
              inputModifier,
            ),
        )
          .labeled(
            _label,
            label(
              _,
              labelStyle,
              `for` := _id,
              labelModifier,
            ),
          ),
      )

    def labelRequired(
        label: String,
        id: String,
        inputModifier: CModifier = PModifier(),
        labelModifier: CModifier = PModifier(),
    ): ModifierAV[A, S, V] =
      labeledGeneric(_.required)(label, id, inputModifier, labelModifier)

    def labelOptional(
        label: String,
        id: String,
        inputModifier: CModifier = PModifier(),
        labelModifier: CModifier = PModifier(),
    ): ModifierAV[A, S, Option[V]] =
      labeledGeneric(identity)(label, id, inputModifier, labelModifier)

  }

  private def genFormInput[V](
      tag: CNodeWidget,
      filterSubmit: KeyboardEvent => Boolean,
  )(implicit decoder: StringDecoder[V]): FormWidget[Submit, String, V] =
    FormWidget(
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
      },
      DefaultStyleSheet.formField.some,
      DefaultStyleSheet.formField.input.some,
      DefaultStyleSheet.formField.label.some,
    )

  def textInput[V: StringDecoder]: FormWidget[Submit, String, V] =
    genFormInput[V](input, e => e.keyCode == KeyCode.Enter.keyCode)

  def textArea[V: StringDecoder]: FormWidget[Submit, String, V] =
    genFormInput[V](textarea, e => e.keyCode == KeyCode.Enter.keyCode && e.ctrlKey)

  def submitButton: CNodeWidgetA[Submit] =
    button(
      DefaultStyleSheet.formSubmit,
      PModifier.builder.withAction[Submit] { rh =>
        onClick := { _ => rh.raise(Raise.Action(Submit)) }
      },
    )

}
