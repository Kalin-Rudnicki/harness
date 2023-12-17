package harness.webUI.widgets

import cats.syntax.either.*
import cats.syntax.option.*
import harness.core.*
import harness.webUI.*
import harness.webUI.style.{given, *}
import harness.webUI.vdom.{given, *}
import org.scalajs.dom.{console, KeyboardEvent}
import zio.Chunk

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

    def decorate(modifiers: ModifierA[A, S]*): FormWidget[A, S, V] =
      copy(widget = widget(modifiers*))

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

  object dropdownSelect {

    final case class Env[V](
        values: Seq[V],
        selected: Option[V],
        expanded: Boolean,
    )
    object Env {

      def initial[V](values: Seq[V]): dropdownSelect.Env[V] = dropdownSelect.Env(values, None, false)
      def initialFirstSelected[V](values: Seq[V]): dropdownSelect.Env[V] = dropdownSelect.Env(values, values.headOption, false)

      def enumInitial[V <: Enum[V]](implicit hec: Enum.HasCompanion[V]): dropdownSelect.Env[V] = Env.initial(hec.companion.enumValues)
      def enumInitialFirstSelected[V <: Enum[V]](implicit hec: Enum.HasCompanion[V]): dropdownSelect.Env[V] = Env.initialFirstSelected(hec.companion.enumValues)

    }

    def apply[V](
        showV: V => CModifier = (_: V).toString,
        closeOnMouseLeave: Boolean = false,
    ): FormWidget[Option[V], dropdownSelect.Env[V], V] =
      FormWidget(
        div(
          DefaultStyleSheet.dropdownSelect,
          PModifier.builder.withAction[Option[V]].withState[dropdownSelect.Env[V]] { (rh, s) =>
            def setSelected(selected: Option[V]): Unit =
              rh.raise(
                Raise.updateState[Env[V]] { s => s.copy(selected = selected, expanded = false) },
                Raise.Action(selected),
              )

            PModifier(
              Option.when(closeOnMouseLeave) {
                onMouseLeave := { _ =>
                  rh.raise(
                    Raise.updateState[Env[V]] { s => s.copy(expanded = false) },
                  )
                }
              },
              div(
                DefaultStyleSheet.dropdownSelect.selected,
                PModifier.foreach(s.selected)(showV),
                onClick := { _ =>
                  rh.updateState[Env[V]](s => s.copy(expanded = !s.expanded))
                },
                onContextMenu := { e =>
                  e.preventDefault()
                  setSelected(None)
                },
              ),
              div(
                DefaultStyleSheet.dropdownSelect.options.visible.when(s.expanded),
                PModifier.foreach(s.values) { v =>
                  div(
                    DefaultStyleSheet.dropdownSelect.option.selected.when(s.selected.contains(v)),
                    showV(v),
                    onClick := { _ => setSelected(v.some) },
                  )
                },
              ),
            )
          },
        ).asValue(_.selected),
        DefaultStyleSheet.formField.some,
        None, // TODO (KR) : ???
        DefaultStyleSheet.formField.label.some,
      )

  }

  def submitButton: CNodeWidgetA[Submit] =
    button(
      DefaultStyleSheet.formSubmit,
      PModifier.builder.withAction[Submit] { rh =>
        onClick := { _ => rh.raise(Raise.Action(Submit)) }
      },
    )

}
