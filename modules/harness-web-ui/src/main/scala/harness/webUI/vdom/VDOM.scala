package harness.webUI.vdom

import cats.data.EitherNel
import cats.syntax.either.*
import harness.webUI.*
import harness.webUI.vdom.widgetModifierFunctions.*
import monocle.Lens
import monocle.macros.GenLens
import scala.scalajs.js
import zio.*
import zio.json.*

trait PModifier[+Action, -StateGet, +StateSet <: StateGet, +Value] extends RaiseFunctions[Action, StateGet, StateSet, Value] { self =>

  // =====| Abstract |=====

  type SelfT[+A, -SG, +SS <: SG, +V] <: PModifier[A, SG, SS, V]

  def build(rh: RaiseHandler[Action, StateSet], state: StateGet): List[rawVDOM.VDom.Modifier]
  val value: StateGet => EitherNel[String, Value]

  protected def mapStateImpl[OuterState, InnerState >: StateSet <: StateGet](
      lens: Lens[OuterState, InnerState],
  ): SelfT[Action, OuterState, OuterState, Value]
  protected def mapValueImpl[StateGet2 <: StateGet, StateSet2 >: StateSet <: StateGet2, Value2](
      f: (StateGet2, EitherNel[String, Value]) => EitherNel[String, Value2],
  ): SelfT[Action, StateGet2, StateSet2, Value2]

  // =====| Mapping State |=====

  inline final def fixState[State >: StateSet <: StateGet]: SelfT[Action, State, State, Value] =
    self.asInstanceOf[SelfT[Action, State, State, Value]]

  inline final def imapState[OuterState, InnerState >: StateSet <: StateGet](lens: Lens[OuterState, InnerState]): SelfT[Action, OuterState, OuterState, Value] =
    self.mapStateImpl[OuterState, InnerState](lens)

  inline final def zoomOut[OuterState](inline f: OuterState => StateGet): SelfT[Action, OuterState, OuterState, Value] =
    self.imapState[OuterState, StateGet](GenLens[OuterState](f).asInstanceOf[Lens[OuterState, StateGet]])

  inline final def zoomOutToPage[State >: StateSet <: StateGet]: SelfT[Action, PageState[State], PageState[State], Value] =
    self.zoomOut[PageState[State]](_.state)

  // =====| Mapping Value |=====

  // --- as ---

  final def as[Value2](f: => Value2): SelfT[Action, StateGet, StateSet, Value2] =
    self.mapValueImpl[StateGet, StateSet, Value2] { (_, _) => f.asRight }

  final def eitherAs[Value2](f: => EitherNel[String, Value2]): SelfT[Action, StateGet, StateSet, Value2] =
    self.mapValueImpl[StateGet, StateSet, Value2] { (_, _) => f }

  inline final def unit: SelfT[Action, StateGet, StateSet, Unit] =
    self.as { () }

  // --- asValue ---

  final def asValue[StateGet2 <: StateGet, StateSet2 >: StateSet <: StateGet2, Value2](f: StateGet2 => Value2): SelfT[Action, StateGet2, StateSet2, Value2] =
    self.mapValueImpl[StateGet2, StateSet2, Value2] { (s, _) => f(s).asRight }

  final def eitherAsValue[StateGet2 <: StateGet, StateSet2 >: StateSet <: StateGet2, Value2](f: StateGet2 => EitherNel[String, Value2]): SelfT[Action, StateGet2, StateSet2, Value2] =
    self.mapValueImpl[StateGet2, StateSet2, Value2] { (s, _) => f(s) }

  // --- mapValue ---

  inline final def mapValue[Value2](f: Value => Value2): SelfT[Action, StateGet, StateSet, Value2] =
    self.mapValueImpl[StateGet, StateSet, Value2] { (_, ev) => ev.map(f) }

  inline final def eitherMapValue[Value2](f: EitherNel[String, Value] => EitherNel[String, Value2]): SelfT[Action, StateGet, StateSet, Value2] =
    self.mapValueImpl[StateGet, StateSet, Value2] { (_, ev) => f(ev) }

  inline final def flatMapValue[Value2](f: Value => EitherNel[String, Value2]): SelfT[Action, StateGet, StateSet, Value2] =
    self.mapValueImpl[StateGet, StateSet, Value2] { (_, ev) => ev.flatMap(f) }

  // --- mapValueS ---

  inline final def mapValueS[StateGet2 <: StateGet, StateSet2 >: StateSet <: StateGet2, Value2](f: (StateGet2, Value) => Value2): SelfT[Action, StateGet2, StateSet2, Value2] =
    self.mapValueImpl[StateGet2, StateSet2, Value2] { (s, ev) => ev.map(f(s, _)) }

  inline final def eitherMapValueS[StateGet2 <: StateGet, StateSet2 >: StateSet <: StateGet2, Value2](
      f: (StateGet2, EitherNel[String, Value]) => EitherNel[String, Value2],
  ): SelfT[Action, StateGet2, StateSet2, Value2] =
    self.mapValueImpl[StateGet2, StateSet2, Value2] { (s, ev) => f(s, ev) }

  inline final def flatMapValueS[StateGet2 <: StateGet, StateSet2 >: StateSet <: StateGet2, Value2](f: (StateGet2, Value) => EitherNel[String, Value2]): SelfT[Action, StateGet2, StateSet2, Value2] =
    self.mapValueImpl[StateGet2, StateSet2, Value2] { (s, ev) => ev.flatMap(f(s, _)) }

  // --- misc ---

  inline final def required[Value2](implicit ev: Value <:< Option[Value2]): SelfT[Action, StateGet, StateSet, Value2] =
    self.flatMapValue[Value2] { v =>
      ev(v) match {
        case Some(v) => v.asRight
        case None    => "Missing required value".leftNel
      }
    }

  // =====| Basic Combinators |=====

  final def <*>[Action2 >: Action, StateGet2 <: StateGet, StateSet2 >: StateSet <: StateGet2, Value2](
      other: PModifier[Action2, StateGet2, StateSet2, Value2],
  )(implicit
      zip: Zippable[Value, Value2],
  ): PModifier[Action2, StateGet2, StateSet2, zip.Out] =
    PModifier.Simple[Action2, StateGet2, StateSet2, zip.Out](
      (rh, state) => self.build(rh, state) ::: other.build(rh, state),
      s =>
        (self.value(s), other.value(s)) match {
          case (Right(v1), Right(v2)) => zip.zip(v1, v2).asRight
          case (Left(e1), Right(_))   => e1.asLeft
          case (Right(_), Left(e2))   => e2.asLeft
          case (Left(e1), Left(e2))   => (e1 ::: e2).asLeft
        },
    )

  final def <*[Action2 >: Action, StateGet2 <: StateGet, StateSet2 >: StateSet <: StateGet2, Value2](
      other: PModifier[Action2, StateGet2, StateSet2, Value2],
  ): PModifier[Action2, StateGet2, StateSet2, Value] =
    PModifier.Simple[Action2, StateGet2, StateSet2, Value](
      (rh, state) => self.build(rh, state) ::: other.build(rh, state),
      self.value,
    )

  final def *>[Action2 >: Action, StateGet2 <: StateGet, StateSet2 >: StateSet <: StateGet2, Value2](
      other: PModifier[Action2, StateGet2, StateSet2, Value2],
  ): PModifier[Action2, StateGet2, StateSet2, Value2] =
    PModifier.Simple[Action2, StateGet2, StateSet2, Value2](
      (rh, state) => self.build(rh, state) ::: other.build(rh, state),
      other.value,
    )

  // =====| Combinators with Value |=====

  final def labeled(labelText: String, buildLabel: String => CModifier = label(_)): PModifier[Action, StateGet, StateSet, Value] =
    PModifier[Action, StateGet, StateSet](
      buildLabel(labelText),
      self,
    ).eitherAsValue[StateGet, StateSet, Value] {
      self.value(_).leftMap {
        _.map { str => s"${labelText.trim.stripSuffix(":")}: $str" }
      }
    }

}
object PModifier {

  def apply(
      modifier0: rawVDOM.VDom.Modifier,
      modifierN: rawVDOM.VDom.Modifier*,
  ): PModifier[Nothing, Any, Nothing, Unit] =
    Simple.const { modifier0 :: modifierN.toList }

  def apply[Action, StateGet, StateSet <: StateGet](
      modifiers: PModifier[Action, StateGet, StateSet, Any]*,
  ): PModifier[Action, StateGet, StateSet, Unit] =
    Simple.unit[Action, StateGet, StateSet] { (rh, state) => modifiers.toList.flatMap(_.build(rh, state)) }

  // =====|  |=====

  def textElement(text: String): PModifier[Nothing, Any, Nothing, Unit] =
    Simple.const { rawVDOM.VDom.TextElement(text) :: Nil }

  def cssAttr(scopedName: rawVDOM.VDom.ScopedName, value: String): CModifier =
    Simple.const { rawVDOM.VDom.CSSAttr(scopedName, value) :: Nil }

  def keyAttr(name: String, value: js.Any): CModifier =
    Simple.const { rawVDOM.VDom.KeyAttr(name, value) :: Nil }

  def stdAttr(scopedName: rawVDOM.VDom.ScopedName, value: String): CModifier =
    Simple.const { rawVDOM.VDom.StdAttr(scopedName, value) :: Nil }

  // =====|  |=====

  def foreach[I, Action, StateGet, StateSet <: StateGet](
      iter: IterableOnce[I],
  )(
      widgetF: I => PModifier[Action, StateGet, StateSet, Any],
  ): PModifier[Action, StateGet, StateSet, Unit] =
    Simple.unit[Action, StateGet, StateSet] { (rh, state) =>
      iter.iterator.flatMap(widgetF(_).build(rh, state)).toList
    }

  def show[S]: Modifier[S] = PModifier.builder.withState[S](_.toString)
  def showJson[S: JsonEncoder]: Modifier[S] = PModifier.builder.withState[S](_.toJsonPretty)

  // =====| Builders |=====

  object builder {

    inline def withAction[A]: BuilderA[A] = new BuilderA[A]
    inline def withRaise: BuilderA[Nothing] = new BuilderA[Nothing]
    inline def withState[S]: BuilderS[S] = new BuilderS[S]

    final class BuilderA[A] {

      inline def withState[S]: BuilderAS[A, S] = new BuilderAS[A, S]

      inline def apply(inline f: RaiseHandler[A, Nothing] => PModifier[A, Any, Nothing, Any]): PModifier[A, Any, Nothing, Unit] =
        Simple.unit[A, Any, Nothing] { (rh, s) => f(rh).build(rh, s) }

    }
    final class BuilderS[S] {

      inline def apply[V](inline f: S => PModifier[Nothing, S, S, V]): PModifier[Nothing, S, S, V] =
        Simple[Nothing, S, S, V](
          (rh, state) => f(state).build(rh, state),
          s => f(s).value(s),
        )

    }
    final class BuilderAS[A, S] {

      inline def apply(inline f: (RaiseHandler[A, S], S) => PModifier[A, S, S, Any]): PModifier[A, S, S, Unit] =
        Simple.unit[A, S, S] { (rh, state) => f(rh, state).build(rh, state) }

    }

  }

  // =====| Simple |=====

  trait Simple[+Action, -StateGet, +StateSet <: StateGet, +Value] extends PModifier[Action, StateGet, StateSet, Value] { self =>

    override final type SelfT[+A, -SG, +SS <: SG, +V] = PModifier[A, SG, SS, V]

    override protected def mapStateImpl[OuterState, InnerState >: StateSet <: StateGet](
        lens: Lens[OuterState, InnerState],
    ): PModifier[Action, OuterState, OuterState, Value] =
      Simple[Action, OuterState, OuterState, Value](
        (rh, state) => self.build(rh.mapState(lens), lens.get(state)),
        s => self.value(lens.get(s)),
      )

    override protected def mapValueImpl[StateGet2 <: StateGet, StateSet2 >: StateSet <: StateGet2, Value2](
        f: (StateGet2, EitherNel[String, Value]) => EitherNel[String, Value2],
    ): PModifier[Action, StateGet2, StateSet2, Value2] =
      Simple[Action, StateGet2, StateSet2, Value2](
        (rh, state) => self.build(rh, state),
        s => f(s, self.value(s)),
      )

  }
  object Simple {

    def apply[Action, StateGet, StateSet <: StateGet, Value](
        _build: (RaiseHandler[Action, StateSet], StateGet) => List[rawVDOM.VDom.Modifier],
        _value: StateGet => EitherNel[String, Value],
    ): Simple[Action, StateGet, StateSet, Value] =
      new Simple[Action, StateGet, StateSet, Value] {
        override def build(rh: RaiseHandler[Action, StateSet], state: StateGet): List[rawVDOM.VDom.Modifier] = _build(rh, state)
        override val value: StateGet => EitherNel[String, Value] = _value
      }

    inline def unit[Action, StateGet, StateSet <: StateGet](
        _build: (RaiseHandler[Action, StateSet], StateGet) => List[rawVDOM.VDom.Modifier],
    ): Simple[Action, StateGet, StateSet, Unit] =
      Simple[Action, StateGet, StateSet, Unit](_build, _ => ().asRight)

    inline def const(
        inline _modifiers: List[rawVDOM.VDom.Modifier],
    ): Simple[Nothing, Any, Nothing, Unit] =
      Simple.unit[Nothing, Any, Nothing] { (_, _) => _modifiers }

  }

}

final case class PNodeWidget[+Action, -StateGet, +StateSet <: StateGet, +Value](
    tagName: String,
    modifiers: List[PModifier[Action, StateGet, StateSet, Any]],
    value: StateGet => EitherNel[String, Value],
) extends PModifier[Action, StateGet, StateSet, Value] { self =>

  override type SelfT[+A, -SG, +SS <: SG, +V] = PNodeWidget[A, SG, SS, V]

  override def build(rh: RaiseHandler[Action, StateSet], state: StateGet): List[rawVDOM.VDom.Modifier] =
    rawVDOM.VDom.NodeElement(
      self.tagName,
      self.modifiers.flatMap(_.build(rh, state)).flatMap(_.toBasics),
    ) :: Nil

  override protected def mapStateImpl[OuterState, InnerState >: StateSet <: StateGet](
      lens: Lens[OuterState, InnerState],
  ): PNodeWidget[Action, OuterState, OuterState, Value] =
    PNodeWidget[Action, OuterState, OuterState, Value](
      tagName = self.tagName,
      modifiers = self.modifiers.map(_.imapState[OuterState, InnerState](lens)),
      value = s => self.value(lens.get(s)),
    )

  override protected def mapValueImpl[StateGet2 <: StateGet, StateSet2 >: StateSet <: StateGet2, Value2](
      f: (StateGet2, EitherNel[String, Value]) => EitherNel[String, Value2],
  ): PNodeWidget[Action, StateGet2, StateSet2, Value2] =
    PNodeWidget[Action, StateGet2, StateSet2, Value2](
      tagName = self.tagName,
      modifiers = self.modifiers,
      value = s => f(s, self.value(s)),
    )

  def apply[Action2 >: Action, StateGet2 <: StateGet, StateSet2 >: StateSet <: StateGet2](
      append: PModifier[Action2, StateGet2, StateSet2, Any]*,
  ): PNodeWidget[Action2, StateGet2, StateSet2, Value] =
    PNodeWidget[Action2, StateGet2, StateSet2, Value](self.tagName, self.modifiers ++ append.toList, self.value)

  def defer[Action2 >: Action, StateGet2 <: StateGet, StateSet2 >: StateSet <: StateGet2, Value2](
      append: PModifier[Action2, StateGet2, StateSet2, Value2],
  ): PNodeWidget[Action2, StateGet2, StateSet2, Value2] =
    PNodeWidget[Action2, StateGet2, StateSet2, Value2](self.tagName, self.modifiers :+ append, append.value)

}
object PNodeWidget {

  inline def apply(tagName: String): PNodeWidget[Nothing, Any, Nothing, Unit] = new PNodeWidget(tagName, Nil, _ => ().asRight)

}
