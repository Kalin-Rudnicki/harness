package harness.webUI.widgets

import cats.data.EitherNel
import cats.syntax.either.*
import cats.syntax.option.*
import harness.webUI.*
import harness.webUI.vdom.{given, *}
import monocle.*
import monocle.macros.GenLens
import scala.annotation.{nowarn, tailrec}
import scala.reflect.ClassTag

object SumWidgets {

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
    SumWidgets[Action, Option[State], Option[Value]](
      Case.subType[Option[State]].filterType[Some[State]].focus(_.value).withWidget {
        some.mapValue(Some(_))
      },
      Case.subType[Option[State]].filterType[None.type].withWidget {
        none.asValue(_ => None)
      },
    )

  def either[Action, LS, RS, LV, RV](
      left: ModifierAV[Action, LS, LV],
      right: ModifierAV[Action, RS, RV],
  ): ModifierAV[Action, Either[LS, RS], Either[LV, RV]] =
    SumWidgets[Action, Either[LS, RS], Either[LV, RV]](
      Case.subType[Either[LS, RS]].filterType[Left[LS, Nothing]].focus(_.value).withWidget {
        left.mapValue(Left(_))
      },
      Case.subType[Either[LS, RS]].filterType[Right[Nothing, RS]].focus(_.value).withWidget {
        right.mapValue(Right(_))
      },
    )

  final case class Case[+Action, OuterState, InnerState, +Value](
      lens: Optional[OuterState, InnerState],
      widget: PModifier[Action, InnerState, InnerState, Value],
  ) {

    private[SumWidgets] def buildOption(
        rh: RaiseHandler[Action, OuterState],
        state: OuterState,
    ): Option[List[rawVDOM.VDom.Modifier]] =
      lens.getOption(state).map { state =>
        widget.build(rh.mapState(lens), state)
      }

    private[SumWidgets] def valueOption(
        state: OuterState,
    ): Option[EitherNel[String, Value]] =
      lens.getOption(state).map { state =>
        widget.value(state)
      }

  }
  object Case {

    final class SubTypeBuilder1[OuterState] {

      inline def filter[InnerState <: OuterState](get: PartialFunction[OuterState, InnerState]): SubTypeBuilder2[OuterState, InnerState] =
        SubTypeBuilder2(Optional(get.lift) { v => _ => v })

      @nowarn
      inline def filterType[InnerState <: OuterState](implicit ct: ClassTag[InnerState]): SubTypeBuilder2[OuterState, InnerState] =
        filter { case ct(is) => is }

    }

    final class SubTypeBuilder2[OuterState, InnerState](
        lens: Optional[OuterState, InnerState],
    ) {

      inline def focus[InnerState2](inline f: InnerState => InnerState2): SubTypeBuilder2[OuterState, InnerState2] =
        SubTypeBuilder2[OuterState, InnerState2](lens.andThen(GenLens[InnerState](f).asInstanceOf[Optional[InnerState, InnerState2]]))

      inline def withWidget[Action, Value](widget: ModifierAV[Action, InnerState, Value]): Case[Action, OuterState, InnerState, Value] =
        Case[Action, OuterState, InnerState, Value](
          lens,
          widget,
        )

    }

    inline def subType[OuterState]: SubTypeBuilder1[OuterState] = new SubTypeBuilder1[OuterState]

    private[SumWidgets] def firstOf[Action, State, Value, O](
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
