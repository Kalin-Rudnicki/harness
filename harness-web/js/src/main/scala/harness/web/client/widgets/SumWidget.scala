package harness.web.client.widgets

import cats.data.EitherNel
import cats.syntax.either.*
import cats.syntax.option.*
import harness.web.client.*
import harness.web.client.vdom.{given, *}
import monocle.*
import scala.annotation.tailrec

object SumWidget {

  def apply[Action, State, Value](
      cases: Case[Action, State, _, Value]*,
  ): ModifierAV[Action, State, Value] = {
    val caseList: List[Case[Action, State, _, Value]] = cases.toList
    PModifier.Simple[Action, State, State, Value](
      (rh, s) => Case.firstOf(caseList)(_.buildOption(rh, s)).getOrElse(Nil),
      s =>
        Case.firstOf(caseList)(_.valueOption(s)) match {
          case Some(value) => value
          case None        => "No case to extract value".leftNel
        },
    )
  }

  def option[Action, State, Value](
      some: ModifierAV[Action, State, Value],
      none: CModifierA[Action] = PModifier(),
  ): ModifierAV[Action, Option[State], Option[Value]] =
    SumWidget[Action, Option[State], Option[Value]](
      Case[Action, Option[State], State, Some[Value]](
        Optional[Option[State], State](identity) { s => _ => s.some },
        some.mapValue(Some(_)),
      ),
      Case[Action, Option[State], None.type, None.type](
        Optional[Option[State], None.type] {
          case None    => None.some
          case Some(_) => None
        } { n => _ => n },
        none.asValue(_ => None),
      ),
    )

  def either[Action, LS, RS, LV, RV](
      left: ModifierAV[Action, LS, LV],
      right: ModifierAV[Action, RS, RV],
  ): ModifierAV[Action, Either[LS, RS], Either[LV, RV]] =
    SumWidget[Action, Either[LS, RS], Either[LV, RV]](
      Case[Action, Either[LS, RS], LS, Left[LV, Nothing]](
        Optional[Either[LS, RS], LS] {
          case Left(value) => value.some
          case Right(_)    => None
        } { s => _ => s.asLeft },
        left.mapValue(Left(_)),
      ),
      Case[Action, Either[LS, RS], RS, Right[Nothing, RV]](
        Optional[Either[LS, RS], RS] {
          case Right(value) => value.some
          case Left(_)      => None
        } { s => _ => s.asRight },
        right.mapValue(Right(_)),
      ),
    )

  final case class Case[+Action, OuterState, InnerState, +Value](
      lens: Optional[OuterState, InnerState],
      widget: PModifier[Action, InnerState, InnerState, Value],
  ) {

    private[SumWidget] def buildOption(
        rh: RaiseHandler[Action, OuterState],
        state: OuterState,
    ): Option[List[rawVDOM.VDom.Modifier]] =
      lens.getOption(state).map { state =>
        widget.build(rh.mapState(lens), state)
      }

    private[SumWidget] def valueOption(
        state: OuterState,
    ): Option[EitherNel[String, Value]] =
      lens.getOption(state).map { state =>
        widget.value(state)
      }

  }
  object Case {

    private[SumWidget] def firstOf[Action, State, Value, O](
        cases: List[Case[Action, State, _, Value]],
    )(f: Case[Action, State, _, Value] => Option[O]): Option[O] = {
      @tailrec
      def loop(cases: List[Case[Action, State, _, Value]]): Option[O] =
        cases match {
          case head :: tail =>
            f(head) match {
              case some @ Some(_) => some
              case None           => loop(tail)
            }
          case Nil => None
        }

      loop(cases)
    }

  }

}
