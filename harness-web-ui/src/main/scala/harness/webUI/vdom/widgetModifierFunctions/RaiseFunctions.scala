package harness.webUI.vdom.widgetModifierFunctions

import cats.data.EitherNel
import cats.syntax.either.*
import harness.http.client.HttpClient
import harness.webUI.*
import harness.webUI.rawVDOM
import harness.webUI.vdom.*
import harness.zio.*
import zio.*

/**
  * Naming paradigm:
  *
  * Action:
  *   - "mapAction" : Action => Action
  *   - "flatMapAction" : Action => Raise
  *   - "mapRaise" : Raise => Raise
  *
  * State :
  *   - "" : Does not take State as an input
  *   - "S" : Takes State as an input
  *
  * Value :
  *   - "" : Does not take Value as an input
  *   - "V" : Takes Value as an input
  *   - "EV" : Takes EitherNel[String, Value] as an input
  *
  * Result :
  *   - "" : Accepts the output type from "Action" section above
  *   - "Z" : Accepts a ZIO of the output type from "Action" section above
  *   - "ZM" : Accepts a List of ZIOs of the output type from "Action" section above
  */
trait RaiseFunctions[+Action, -StateGet, +StateSet <: StateGet, +Value] { self: PModifier[Action, StateGet, StateSet, Value] =>

  // =====| mapAction |=====

  // ----- noValue -----

  // --- noState ---

  inline final def mapAction[Action2](
      inline f: Action => Action2,
  ): PModifier[Action2, StateGet, StateSet, Value] =
    self.mapActionZM { r => ZIO.succeed(f(r) :: Nil) }

  inline final def mapActionZ[Action2](
      inline f: Action => SHRIO[HttpClient.ClientT, Action2],
  ): PModifier[Action2, StateGet, StateSet, Value] =
    self.mapActionZM(f(_).map(_ :: Nil))

  inline final def mapActionZM[Action2](
      inline f: Action => SHRIO[HttpClient.ClientT, List[Action2]],
  ): PModifier[Action2, StateGet, StateSet, Value] =
    self.mapActionSVZM[StateGet, StateSet, Action2] { (r, _, _) => f(r) }

  // --- state ---

  inline final def mapActionS[StateGet2 <: StateGet, StateSet2 >: StateSet <: StateGet2, Action2](
      inline f: (Action, StateGet2) => Action2,
  ): PModifier[Action2, StateGet2, StateSet2, Value] =
    self.mapActionSZM[StateGet2, StateSet2, Action2] { (r, s) => ZIO.succeed(f(r, s) :: Nil) }

  inline final def mapActionSZ[StateGet2 <: StateGet, StateSet2 >: StateSet <: StateGet2, Action2](
      inline f: (Action, StateGet2) => SHRIO[HttpClient.ClientT, Action2],
  ): PModifier[Action2, StateGet2, StateSet2, Value] =
    self.mapActionSZM[StateGet2, StateSet2, Action2](f(_, _).map(_ :: Nil))

  inline final def mapActionSZM[StateGet2 <: StateGet, StateSet2 >: StateSet <: StateGet2, Action2](
      inline f: (Action, StateGet2) => SHRIO[HttpClient.ClientT, List[Action2]],
  ): PModifier[Action2, StateGet2, StateSet2, Value] =
    self.mapActionSVZM[StateGet2, StateSet2, Action2] { (r, s, _) => f(r, s) }

  // ----- value -----

  // --- noState ---

  inline final def mapActionV[Action2](
      inline f: (Action, Value) => Action2,
  ): PModifier[Action2, StateGet, StateSet, Value] =
    self.mapActionVZM { (r, v) => ZIO.succeed(f(r, v) :: Nil) }

  inline final def mapActionVZ[Action2](
      inline f: (Action, Value) => SHRIO[HttpClient.ClientT, Action2],
  ): PModifier[Action2, StateGet, StateSet, Value] =
    self.mapActionVZM(f(_, _).map(_ :: Nil))

  inline final def mapActionVZM[Action2](
      inline f: (Action, Value) => SHRIO[HttpClient.ClientT, List[Action2]],
  ): PModifier[Action2, StateGet, StateSet, Value] =
    self.mapActionSVZM[StateGet, StateSet, Action2] { (r, _, v) => f(r, v) }

  // --- state ---

  inline final def mapActionSV[StateGet2 <: StateGet, StateSet2 >: StateSet <: StateGet2, Action2](
      inline f: (Action, StateGet2, Value) => Action2,
  ): PModifier[Action2, StateGet2, StateSet2, Value] =
    self.mapActionSVZM[StateGet2, StateSet2, Action2] { (r, s, v) => ZIO.succeed(f(r, s, v) :: Nil) }

  inline final def mapActionSVZ[StateGet2 <: StateGet, StateSet2 >: StateSet <: StateGet2, Action2](
      inline f: (Action, StateGet2, Value) => SHRIO[HttpClient.ClientT, Action2],
  ): PModifier[Action2, StateGet2, StateSet2, Value] =
    self.mapActionSVZM[StateGet2, StateSet2, Action2](f(_, _, _).map(_ :: Nil))

  inline final def mapActionSVZM[StateGet2 <: StateGet, StateSet2 >: StateSet <: StateGet2, Action2](
      inline f: (Action, StateGet2, Value) => SHRIO[HttpClient.ClientT, List[Action2]],
  ): PModifier[Action2, StateGet2, StateSet2, Value] =
    self.mapActionSEVZM[StateGet2, StateSet2, Action2] {
      case (r, s, Right(v))     => f(r, s, v)
      case (_, _, Left(errors)) => ZIO.hFailUserErrors(errors)
    }

  // ----- eitherValue -----

  // --- noState ---

  // ignoring: mapActionEV

  inline final def mapActionEVZ[Action2](
      inline f: (Action, EitherNel[String, Value]) => SHRIO[HttpClient.ClientT, Action2],
  ): PModifier[Action2, StateGet, StateSet, Value] =
    self.mapActionEVZM(f(_, _).map(_ :: Nil))

  inline final def mapActionEVZM[Action2](
      inline f: (Action, EitherNel[String, Value]) => SHRIO[HttpClient.ClientT, List[Action2]],
  ): PModifier[Action2, StateGet, StateSet, Value] =
    self.mapActionSEVZM[StateGet, StateSet, Action2] { (r, _, v) => f(r, v) }

  // --- state ---

  // ignoring: mapActionSEV

  inline final def mapActionSEVZ[StateGet2 <: StateGet, StateSet2 >: StateSet <: StateGet2, Action2](
      inline f: (Action, StateGet2, EitherNel[String, Value]) => SHRIO[HttpClient.ClientT, Action2],
  ): PModifier[Action2, StateGet2, StateSet2, Value] =
    self.mapActionSEVZM[StateGet2, StateSet2, Action2](f(_, _, _).map(_ :: Nil))

  inline final def mapActionSEVZM[StateGet2 <: StateGet, StateSet2 >: StateSet <: StateGet2, Action2](
      inline f: (Action, StateGet2, EitherNel[String, Value]) => SHRIO[HttpClient.ClientT, List[Action2]],
  ): PModifier[Action2, StateGet2, StateSet2, Value] =
    self.mapRaiseSEVZM[StateGet2, StateSet2, Action2] {
      case (Raise.Action(r), s, v)                        => f(r, s, v).map(_.map(Raise.Action(_)))
      case (sou: Raise.StandardOrUpdate[StateSet2], _, _) => ZIO.succeed(sou :: Nil)
    }

  // =====| flatMapAction |=====

  // ----- noValue -----

  // --- noState ---

  inline final def flatMapAction[StateGet2 <: StateGet, StateSet2 >: StateSet <: StateGet2, Action2](
      inline f: Action => Raise[Action2, StateSet2],
  ): PModifier[Action2, StateGet2, StateSet2, Value] =
    self.flatMapActionZM[StateGet2, StateSet2, Action2] { r => ZIO.succeed(f(r) :: Nil) }

  inline final def flatMapActionZ[StateGet2 <: StateGet, StateSet2 >: StateSet <: StateGet2, Action2](
      inline f: Action => SHRIO[HttpClient.ClientT, Raise[Action2, StateSet2]],
  ): PModifier[Action2, StateGet2, StateSet2, Value] =
    self.flatMapActionZM(f(_).map(_ :: Nil))

  inline final def flatMapActionZM[StateGet2 <: StateGet, StateSet2 >: StateSet <: StateGet2, Action2](
      inline f: Action => SHRIO[HttpClient.ClientT, List[Raise[Action2, StateSet2]]],
  ): PModifier[Action2, StateGet2, StateSet2, Value] =
    self.flatMapActionSVZM[StateGet2, StateSet2, Action2] { (r, _, _) => f(r) }

  // --- state ---

  inline final def flatMapActionS[StateGet2 <: StateGet, StateSet2 >: StateSet <: StateGet2, Action2](
      inline f: (Action, StateGet2) => Raise[Action2, StateSet2],
  ): PModifier[Action2, StateGet2, StateSet2, Value] =
    self.flatMapActionSZM[StateGet2, StateSet2, Action2] { (r, s) => ZIO.succeed(f(r, s) :: Nil) }

  inline final def flatMapActionSZ[StateGet2 <: StateGet, StateSet2 >: StateSet <: StateGet2, Action2](
      inline f: (Action, StateGet2) => SHRIO[HttpClient.ClientT, Raise[Action2, StateSet2]],
  ): PModifier[Action2, StateGet2, StateSet2, Value] =
    self.flatMapActionSZM(f(_, _).map(_ :: Nil))

  inline final def flatMapActionSZM[StateGet2 <: StateGet, StateSet2 >: StateSet <: StateGet2, Action2](
      inline f: (Action, StateGet2) => SHRIO[HttpClient.ClientT, List[Raise[Action2, StateSet2]]],
  ): PModifier[Action2, StateGet2, StateSet2, Value] =
    self.flatMapActionSVZM[StateGet2, StateSet2, Action2] { (r, s, _) => f(r, s) }

  // ----- value -----

  // --- noState ---

  inline final def flatMapActionV[StateGet2 <: StateGet, StateSet2 >: StateSet <: StateGet2, Action2](
      inline f: (Action, Value) => Raise[Action2, StateSet2],
  ): PModifier[Action2, StateGet2, StateSet2, Value] =
    self.flatMapActionVZM[StateGet2, StateSet2, Action2] { (r, v) => ZIO.succeed(f(r, v) :: Nil) }

  inline final def flatMapActionVZ[StateGet2 <: StateGet, StateSet2 >: StateSet <: StateGet2, Action2](
      inline f: (Action, Value) => SHRIO[HttpClient.ClientT, Raise[Action2, StateSet2]],
  ): PModifier[Action2, StateGet2, StateSet2, Value] =
    self.flatMapActionVZM(f(_, _).map(_ :: Nil))

  inline final def flatMapActionVZM[StateGet2 <: StateGet, StateSet2 >: StateSet <: StateGet2, Action2](
      inline f: (Action, Value) => SHRIO[HttpClient.ClientT, List[Raise[Action2, StateSet2]]],
  ): PModifier[Action2, StateGet2, StateSet2, Value] =
    self.flatMapActionSVZM[StateGet2, StateSet2, Action2] { (r, _, v) => f(r, v) }

  // --- state ---

  inline final def flatMapActionSV[StateGet2 <: StateGet, StateSet2 >: StateSet <: StateGet2, Action2](
      inline f: (Action, StateGet2, Value) => Raise[Action2, StateSet2],
  ): PModifier[Action2, StateGet2, StateSet2, Value] =
    self.flatMapActionSVZM[StateGet2, StateSet2, Action2] { (r, s, v) => ZIO.succeed(f(r, s, v) :: Nil) }

  inline final def flatMapActionSVZ[StateGet2 <: StateGet, StateSet2 >: StateSet <: StateGet2, Action2](
      inline f: (Action, StateGet2, Value) => SHRIO[HttpClient.ClientT, Raise[Action2, StateSet2]],
  ): PModifier[Action2, StateGet2, StateSet2, Value] =
    self.flatMapActionSVZM(f(_, _, _).map(_ :: Nil))

  inline final def flatMapActionSVZM[StateGet2 <: StateGet, StateSet2 >: StateSet <: StateGet2, Action2](
      inline f: (Action, StateGet2, Value) => SHRIO[HttpClient.ClientT, List[Raise[Action2, StateSet2]]],
  ): PModifier[Action2, StateGet2, StateSet2, Value] =
    self.flatMapActionSEVZM[StateGet2, StateSet2, Action2] {
      case (r, s, Right(v))     => f(r, s, v)
      case (_, _, Left(errors)) => ZIO.hFailUserErrors(errors)
    }

  // ----- eitherValue -----

  // --- noState ---

  // ignoring: flatMapActionEV

  inline final def flatMapActionEVZ[StateGet2 <: StateGet, StateSet2 >: StateSet <: StateGet2, Action2](
      inline f: (Action, EitherNel[String, Value]) => SHRIO[HttpClient.ClientT, Raise[Action2, StateSet2]],
  ): PModifier[Action2, StateGet2, StateSet2, Value] =
    self.flatMapActionEVZM(f(_, _).map(_ :: Nil))

  inline final def flatMapActionEVZM[StateGet2 <: StateGet, StateSet2 >: StateSet <: StateGet2, Action2](
      inline f: (Action, EitherNel[String, Value]) => SHRIO[HttpClient.ClientT, List[Raise[Action2, StateSet2]]],
  ): PModifier[Action2, StateGet2, StateSet2, Value] =
    self.flatMapActionSEVZM[StateGet2, StateSet2, Action2] { (r, _, v) => f(r, v) }

  // --- state ---

  // ignoring: flatMapActionSEV

  inline final def flatMapActionSEVZ[StateGet2 <: StateGet, StateSet2 >: StateSet <: StateGet2, Action2](
      inline f: (Action, StateGet2, EitherNel[String, Value]) => SHRIO[HttpClient.ClientT, Raise[Action2, StateSet2]],
  ): PModifier[Action2, StateGet2, StateSet2, Value] =
    self.flatMapActionSEVZM(f(_, _, _).map(_ :: Nil))

  inline final def flatMapActionSEVZM[StateGet2 <: StateGet, StateSet2 >: StateSet <: StateGet2, Action2](
      inline f: (Action, StateGet2, EitherNel[String, Value]) => SHRIO[HttpClient.ClientT, List[Raise[Action2, StateSet2]]],
  ): PModifier[Action2, StateGet2, StateSet2, Value] =
    self.mapRaiseSEVZM[StateGet2, StateSet2, Action2] {
      case (Raise.Action(r), s, v)                        => f(r, s, v)
      case (sou: Raise.StandardOrUpdate[StateSet2], _, _) => ZIO.succeed(sou :: Nil)
    }

  // =====| mapRaise |=====

  // ----- noValue -----

  // --- noState ---

  inline final def mapRaise[StateGet2 <: StateGet, StateSet2 >: StateSet <: StateGet2, Action2](
      inline f: Raise[Action, StateSet] => Raise[Action2, StateSet2],
  ): PModifier[Action2, StateGet2, StateSet2, Value] =
    self.mapRaiseZM[StateGet2, StateSet2, Action2] { r => ZIO.succeed(f(r) :: Nil) }

  inline final def mapRaiseZ[StateGet2 <: StateGet, StateSet2 >: StateSet <: StateGet2, Action2](
      inline f: Raise[Action, StateSet] => SHRIO[HttpClient.ClientT, Raise[Action2, StateSet2]],
  ): PModifier[Action2, StateGet2, StateSet2, Value] =
    self.mapRaiseZM[StateGet2, StateSet2, Action2](f(_).map(_ :: Nil))

  inline final def mapRaiseZM[StateGet2 <: StateGet, StateSet2 >: StateSet <: StateGet2, Action2](
      inline f: Raise[Action, StateSet] => SHRIO[HttpClient.ClientT, List[Raise[Action2, StateSet2]]],
  ): PModifier[Action2, StateGet2, StateSet2, Value] =
    self.mapRaiseSVZM[StateGet2, StateSet2, Action2] { (r, _, _) => f(r) }

  // --- state ---

  inline final def mapRaiseS[StateGet2 <: StateGet, StateSet2 >: StateSet <: StateGet2, Action2](
      inline f: (Raise[Action, StateSet], StateGet2) => Raise[Action2, StateSet2],
  ): PModifier[Action2, StateGet2, StateSet2, Value] =
    self.mapRaiseSZM[StateGet2, StateSet2, Action2] { (r, s) => ZIO.succeed(f(r, s) :: Nil) }

  inline final def mapRaiseSZ[StateGet2 <: StateGet, StateSet2 >: StateSet <: StateGet2, Action2](
      inline f: (Raise[Action, StateSet], StateGet2) => SHRIO[HttpClient.ClientT, Raise[Action2, StateSet2]],
  ): PModifier[Action2, StateGet2, StateSet2, Value] =
    self.mapRaiseSZM[StateGet2, StateSet2, Action2](f(_, _).map(_ :: Nil))

  inline final def mapRaiseSZM[StateGet2 <: StateGet, StateSet2 >: StateSet <: StateGet2, Action2](
      inline f: (Raise[Action, StateSet], StateGet2) => SHRIO[HttpClient.ClientT, List[Raise[Action2, StateSet2]]],
  ): PModifier[Action2, StateGet2, StateSet2, Value] =
    self.mapRaiseSVZM[StateGet2, StateSet2, Action2] { (r, s, _) => f(r, s) }

  // ----- value -----

  // --- noState ---

  inline final def mapRaiseV[StateGet2 <: StateGet, StateSet2 >: StateSet <: StateGet2, Action2](
      inline f: (Raise[Action, StateSet], Value) => Raise[Action2, StateSet2],
  ): PModifier[Action2, StateGet2, StateSet2, Value] =
    self.mapRaiseVZM[StateGet2, StateSet2, Action2] { (r, v) => ZIO.succeed(f(r, v) :: Nil) }

  inline final def mapRaiseVZ[StateGet2 <: StateGet, StateSet2 >: StateSet <: StateGet2, Action2](
      inline f: (Raise[Action, StateSet], Value) => SHRIO[HttpClient.ClientT, Raise[Action2, StateSet2]],
  ): PModifier[Action2, StateGet2, StateSet2, Value] =
    self.mapRaiseVZM[StateGet2, StateSet2, Action2](f(_, _).map(_ :: Nil))

  inline final def mapRaiseVZM[StateGet2 <: StateGet, StateSet2 >: StateSet <: StateGet2, Action2](
      inline f: (Raise[Action, StateSet], Value) => SHRIO[HttpClient.ClientT, List[Raise[Action2, StateSet2]]],
  ): PModifier[Action2, StateGet2, StateSet2, Value] =
    self.mapRaiseSVZM[StateGet2, StateSet2, Action2] { (r, _, v) => f(r, v) }

  // --- state ---

  inline final def mapRaiseSV[StateGet2 <: StateGet, StateSet2 >: StateSet <: StateGet2, Action2](
      inline f: (Raise[Action, StateSet], StateGet2, Value) => Raise[Action2, StateSet2],
  ): PModifier[Action2, StateGet2, StateSet2, Value] =
    self.mapRaiseSVZM[StateGet2, StateSet2, Action2] { (r, s, v) => ZIO.succeed(f(r, s, v) :: Nil) }

  inline final def mapRaiseSVZ[StateGet2 <: StateGet, StateSet2 >: StateSet <: StateGet2, Action2](
      inline f: (Raise[Action, StateSet], StateGet2, Value) => SHRIO[HttpClient.ClientT, Raise[Action2, StateSet2]],
  ): PModifier[Action2, StateGet2, StateSet2, Value] =
    self.mapRaiseSVZM[StateGet2, StateSet2, Action2](f(_, _, _).map(_ :: Nil))

  inline final def mapRaiseSVZM[StateGet2 <: StateGet, StateSet2 >: StateSet <: StateGet2, Action2](
      inline f: (Raise[Action, StateSet], StateGet2, Value) => SHRIO[HttpClient.ClientT, List[Raise[Action2, StateSet2]]],
  ): PModifier[Action2, StateGet2, StateSet2, Value] =
    self.mapRaiseSEVZM[StateGet2, StateSet2, Action2] {
      case (r, s, Right(v))     => f(r, s, v)
      case (_, _, Left(errors)) => ZIO.hFailUserErrors(errors)
    }

  // ----- eitherValue -----

  // --- noState ---

  // ignoring: mapRaiseEV

  inline final def mapRaiseEVZ[StateGet2 <: StateGet, StateSet2 >: StateSet <: StateGet2, Action2](
      inline f: (Raise[Action, StateSet], EitherNel[String, Value]) => SHRIO[HttpClient.ClientT, Raise[Action2, StateSet2]],
  ): PModifier[Action2, StateGet2, StateSet2, Value] =
    self.mapRaiseSEVZM[StateGet2, StateSet2, Action2] { (r, _, v) => f(r, v).map(_ :: Nil) }

  inline final def mapRaiseEVZM[StateGet2 <: StateGet, StateSet2 >: StateSet <: StateGet2, Action2](
      inline f: (Raise[Action, StateSet], EitherNel[String, Value]) => SHRIO[HttpClient.ClientT, List[Raise[Action2, StateSet2]]],
  ): PModifier[Action2, StateGet2, StateSet2, Value] =
    self.mapRaiseSEVZM[StateGet2, StateSet2, Action2] { (r, _, v) => f(r, v) }

  // --- state ---

  // ignoring: mapRaiseSEV

  inline final def mapRaiseSEVZ[StateGet2 <: StateGet, StateSet2 >: StateSet <: StateGet2, Action2](
      inline f: (Raise[Action, StateSet], StateGet2, EitherNel[String, Value]) => SHRIO[HttpClient.ClientT, Raise[Action2, StateSet2]],
  ): PModifier[Action2, StateGet2, StateSet2, Value] =
    self.mapRaiseSEVZM(f(_, _, _).map(_ :: Nil))

  final def mapRaiseSEVZM[StateGet2 <: StateGet, StateSet2 >: StateSet <: StateGet2, Action2](
      f: (Raise[Action, StateSet], StateGet2, EitherNel[String, Value]) => SHRIO[HttpClient.ClientT, List[Raise[Action2, StateSet2]]],
  ): PModifier[Action2, StateGet2, StateSet2, Value] =
    new PModifier.Simple[Action2, StateGet2, StateSet2, Value] {
      override def build(rh: RaiseHandler[Action2, StateSet2], state: StateGet2): List[rawVDOM.VDom.Modifier] =
        self.build(
          rh.mapRaise[Action, StateSet] { r => f(r, state, self.value(state)) },
          state,
        )

      override val value: StateGet2 => EitherNel[String, Value] = self.value
    }

}
