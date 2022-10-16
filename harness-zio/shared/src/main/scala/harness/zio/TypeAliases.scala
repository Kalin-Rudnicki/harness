package harness.zio

import cats.data.NonEmptyList
import harness.core.*
import zio.*
import zio.stream.*

// =====| Harness Types |=====

// format: off
type  HTask[    +A] = ZIO[Any,            HError, A]
type   HRIO[-R, +A] = ZIO[R,              HError, A]
type SHTask[    +A] = ZIO[HarnessEnv,     HError, A]
type  SHRIO[-R, +A] = ZIO[HarnessEnv & R, HError, A]

type  HTaskLayer[    +A] = ZLayer[Any,            HError, A]
type     HRLayer[-R, +A] = ZLayer[R,              HError, A]
type SHTaskLayer[    +A] = ZLayer[HarnessEnv,     HError, A]
type    SHRLayer[-R, +A] = ZLayer[HarnessEnv & R, HError, A]

type  HTaskStream[    +A] = ZStream[Any,            HError, A]
type     HRStream[-R, +A] = ZStream[R,              HError, A]
type SHTaskStream[    +A] = ZStream[HarnessEnv,     HError, A]
type    SHRStream[-R, +A] = ZStream[HarnessEnv & R, HError, A]
// format: on

// =====| Missing Types for ZStream |=====

// format: off
// TODO (KR) : REMOVE if ZIO adds these 
type    RStream[-R, +A] = ZStream[R,   Throwable, A]
type   URStream[-R, +A] = ZStream[R,   Nothing,   A]
type TaskStream[    +A] = ZStream[Any, Throwable, A]
// format: on
