package harness.webUI.widgets

import cats.syntax.either.*
import cats.syntax.option.*
import harness.core.*
import harness.webUI.*
import harness.webUI.vdom.{given, *}
import org.scalajs.dom.{console, KeyboardEvent}

sealed trait SubmitOr[+A]

type Submit = Submit.type
case object Submit extends SubmitOr[Nothing]

final case class SubmitOrAction[+A](action: A) extends SubmitOr[A]

// TODO (KR) :
/*
sealed trait UpdateOn
object UpdateOn {
  final case class KeyUp(timeout: Option[Int]) extends UpdateOn
  case object Blur extends UpdateOn
}
 */

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
