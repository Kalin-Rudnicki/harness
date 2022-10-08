package harness.web.client.widgets

import cats.data.EitherNel
import cats.syntax.either.*
import cats.syntax.option.*
import harness.web.client.*
import harness.web.client.vdom.{given, *}
import monocle.*
import monocle.macros.GenLens
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
      Case.subTypeThen[Action, Option[State], Some[State], State, Some[Value]] { case Some(value) => Some(value) }(_.value) {
        some.mapValue(Some(_))
      },
      Case.subType[Action, Option[State], None.type, None.type] { case None => None } {
        none.asValue(_ => None)
      },
    )

  def either[Action, LS, RS, LV, RV](
      left: ModifierAV[Action, LS, LV],
      right: ModifierAV[Action, RS, RV],
  ): ModifierAV[Action, Either[LS, RS], Either[LV, RV]] =
    SumWidget[Action, Either[LS, RS], Either[LV, RV]](
      Case.subTypeThen[Action, Either[LS, RS], Left[LS, Nothing], LS, Left[LV, Nothing]] { case Left(value) => Left(value) }(_.value) {
        left.mapValue(Left(_))
      },
      Case.subTypeThen[Action, Either[LS, RS], Right[Nothing, RS], RS, Right[Nothing, RV]] { case Right(value) => Right(value) }(_.value) {
        right.mapValue(Right(_))
      },
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

    def subType[Action, OuterState, InnerState <: OuterState, Value](
        get: PartialFunction[OuterState, InnerState],
    )(
        widget: ModifierAV[Action, InnerState, Value],
    ): Case[Action, OuterState, InnerState, Value] =
      Case[Action, OuterState, InnerState, Value](
        Optional(get.lift) { v => _ => v },
        widget,
      )

    inline def subTypeThen[Action, OuterState, TmpInnerState <: OuterState, InnerState, Value](
        get: PartialFunction[OuterState, TmpInnerState],
    )(
        inline f: TmpInnerState => InnerState,
    )(
        widget: ModifierAV[Action, InnerState, Value],
    ): Case[Action, OuterState, InnerState, Value] =
      Case[Action, OuterState, InnerState, Value](
        Optional(get.lift) { v => _ => v }.andThen(GenLens[TmpInnerState](f).asInstanceOf[Optional[TmpInnerState, InnerState]]),
        widget,
      )

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
