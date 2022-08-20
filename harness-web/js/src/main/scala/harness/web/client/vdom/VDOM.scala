package harness.web.client.vdom

import cats.data.EitherNel
import cats.syntax.either.*
import cats.syntax.parallel.*
import harness.core.*
import harness.web.client.RaiseHandler
import harness.web.client.rawVDOM
import monocle.Lens
import monocle.macros.GenLens
import zio.*

trait PModifier[+Action, -StateGet, +StateSet <: StateGet] {
  def build(rh: RaiseHandler[Action, StateSet], state: StateGet): List[rawVDOM.VDom.Modifier]
}
object PModifier {

  def apply(modifiers: rawVDOM.VDom.Modifier*): PModifier[Nothing, Any, Nothing] = (_, _) => modifiers.toList

  final class RHBuilder[+A, S] private[PModifier] {
    def apply[A2 >: A](f: RaiseHandler[A2, S] => ModifierA[A2, S]): ModifierA[A2, S] =
      new ModifierA[A2, S] {
        override def build(rh: RaiseHandler[A2, S], state: S): List[rawVDOM.VDom.Modifier] = f(rh).build(rh, state)
      }
  }

  final class SBuilder[S] private[PModifier] {
    def apply[A](f: S => ModifierA[A, S]): ModifierA[A, S] =
      new ModifierA[A, S] {
        override def build(rh: RaiseHandler[A, S], state: S): List[rawVDOM.VDom.Modifier] = f(state).build(rh, state)
      }
  }

  def withRH[S]: RHBuilder[Nothing, S] = new RHBuilder[Nothing, S]
  def withRHA[A, S]: RHBuilder[A, S] = new RHBuilder[A, S]
  def withS[S]: SBuilder[S] = new SBuilder[S]

  def show[S]: Modifier[S] = PModifier.withS[S](_.toString)

}

trait PWidget[+Action, -StateGet, +StateSet <: StateGet, +Value] extends PModifier[Action, StateGet, StateSet] { self =>

  override def build(rh: RaiseHandler[Action, StateSet], state: StateGet): List[rawVDOM.VDom.Element]

  def mapValueSE[State >: StateSet <: StateGet, Value2](
      f: (State, EitherNel[HError, Value]) => EitherNel[HError, Value2],
  ): WidgetAV[Action, State, Value2] =
    new WidgetAV[Action, State, Value2] {
      override def build(rh: RaiseHandler[Action, State], state: State): List[rawVDOM.VDom.Element] = self.build(rh, state)
      override val value: State => EitherNel[HError, Value2] = s => f(s, self.value(s))
    }

  def imapState[OuterState, InnerState >: StateSet <: StateGet](
      lens: Lens[OuterState, InnerState],
  ): PWidget[Action, OuterState, OuterState, Value] =
    new PWidget[Action, OuterState, OuterState, Value] {
      override def build(rh: RaiseHandler[Action, OuterState], state: OuterState): List[rawVDOM.VDom.Element] = self.build(rh.mapState(lens), lens.get(state))
      override val value: OuterState => EitherNel[HError, Value] = state => self.value(lens.get(state))
    }

  inline def zoomOut[OuterState](inline f: OuterState => StateGet): PWidget[Action, OuterState, OuterState, Value] =
    imapState[OuterState, StateGet](GenLens[OuterState](f).asInstanceOf[Lens[OuterState, StateGet]])

  val value: StateGet => EitherNel[HError, Value]

  // =====| Zip |=====

  final def <*>[Action2 >: Action, StateGet2 <: StateGet, StateSet2 >: StateSet <: StateGet2, Value2](other: PWidget[Action2, StateGet2, StateSet2, Value2])(implicit
      zip: Zippable[Value, Value2],
  ): PWidget[Action2, StateGet2, StateSet2, zip.Out] =
    new PWidget[Action2, StateGet2, StateSet2, zip.Out] {
      override def build(rh: RaiseHandler[Action2, StateSet2], state: StateGet2): List[rawVDOM.VDom.Element] =
        self.build(rh, state) ++ other.build(rh, state)
      override val value: StateGet2 => EitherNel[HError, zip.Out] = s => (self.value(s), other.value(s)).parMapN(zip.zip)
    }

  final def <*[Action2 >: Action, StateGet2 <: StateGet, StateSet2 >: StateSet <: StateGet2, Value2](
      other: PWidget[Action2, StateGet2, StateSet2, Value2],
  ): PWidget[Action2, StateGet2, StateSet2, Value] =
    new PWidget[Action2, StateGet2, StateSet2, Value] {
      override def build(rh: RaiseHandler[Action2, StateSet2], state: StateGet2): List[rawVDOM.VDom.Element] =
        self.build(rh, state) ++ other.build(rh, state)
      override val value: StateGet2 => EitherNel[HError, Value] = self.value
    }

  final def *>[Action2 >: Action, StateGet2 <: StateGet, StateSet2 >: StateSet <: StateGet2, Value2](
      other: PWidget[Action2, StateGet2, StateSet2, Value2],
  ): PWidget[Action2, StateGet2, StateSet2, Value2] =
    new PWidget[Action2, StateGet2, StateSet2, Value2] {
      override def build(rh: RaiseHandler[Action2, StateSet2], state: StateGet2): List[rawVDOM.VDom.Element] =
        self.build(rh, state) ++ other.build(rh, state)
      override val value: StateGet2 => EitherNel[HError, Value2] = other.value
    }

  // =====| Map Raise |=====

  // TODO (KR) :
  // def mapRaiseSVE

  // =====| MapValue |=====

}
object PWidget {

  def apply[Action, StateGet, StateSet <: StateGet](widgets: PWidget[Action, StateGet, StateSet, Any]*): PWidget[Action, StateGet, StateSet, Unit] =
    new PWidget[Action, StateGet, StateSet, Unit] {
      override def build(rh: RaiseHandler[Action, StateSet], state: StateGet): List[rawVDOM.VDom.Element] = widgets.toList.flatMap(_.build(rh, state))
      override val value: StateGet => EitherNel[HError, Unit] = _ => ().asRight
    }

  def foreach[I, Action, StateGet, StateSet <: StateGet](iter: IterableOnce[I])(widgetF: I => PWidget[Action, StateGet, StateSet, Any]): PWidget[Action, StateGet, StateSet, Unit] =
    new PWidget[Action, StateGet, StateSet, Unit] {
      override def build(rh: RaiseHandler[Action, StateSet], state: StateGet): List[rawVDOM.VDom.Element] = iter.iterator.flatMap { widgetF(_).build(rh, state) }.toList
      override val value: StateGet => EitherNel[HError, Unit] = _ => ().asRight
    }

}

final case class TextWidget(text: String) extends PWidget[Nothing, Any, Nothing, Unit] {
  override def build(rh: RaiseHandler[Nothing, Nothing], state: Any): List[rawVDOM.VDom.Element] =
    rawVDOM.VDom.TextElement(text) :: Nil
  override val value: Any => EitherNel[HError, Unit] = _ => ().asRight
}

final case class PNodeWidget[+Action, -StateGet, +StateSet <: StateGet, +Value](
    tagName: String,
    modifiers: List[PModifier[Action, StateGet, StateSet]],
    value: StateGet => EitherNel[HError, Value],
) extends PWidget[Action, StateGet, StateSet, Value] { self =>

  override def build(rh: RaiseHandler[Action, StateSet], state: StateGet): List[rawVDOM.VDom.Element] =
    rawVDOM.VDom.NodeElement(
      tagName,
      modifiers.flatMap(_.build(rh, state)).flatMap(_.toBasics),
    ) :: Nil

  def apply[Action2 >: Action, StateGet2 <: StateGet, StateSet2 >: StateSet <: StateGet2](
      append: PModifier[Action2, StateGet2, StateSet2]*,
  ): PNodeWidget[Action2, StateGet2, StateSet2, Value] =
    PNodeWidget[Action2, StateGet2, StateSet2, Value](self.tagName, self.modifiers ++ append.toList, self.value)

  def deferValue[Action2 >: Action, StateGet2 <: StateGet, StateSet2 >: StateSet <: StateGet2, Value2](
      deferTo: PWidget[Action2, StateGet2, StateSet2, Value2],
  ): PNodeWidget[Action2, StateGet2, StateSet2, Value2] =
    PNodeWidget[Action2, StateGet2, StateSet2, Value2](self.tagName, self.modifiers :+ deferTo, deferTo.value)

  override def mapValueSE[State >: StateSet <: StateGet, Value2](
      f: (State, EitherNel[HError, Value]) => EitherNel[HError, Value2],
  ): NodeWidgetAV[Action, State, Value2] =
    new NodeWidgetAV[Action, State, Value2](self.tagName, self.modifiers, s => f(s, self.value(s)))

}
object PNodeWidget {

  def apply(tagName: String): CNodeWidget = PNodeWidget(tagName, Nil, _ => ().asRight)

}
