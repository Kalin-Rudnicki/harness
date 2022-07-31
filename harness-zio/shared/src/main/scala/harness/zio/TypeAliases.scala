package harness.zio

import cats.data.NonEmptyList
import harness.core.*
import zio.*

// TODO (KR) : add to this ENV
type HarnessEnv = Any

type HTask[A] = IO[KError, A]
type HTaskN[A] = IO[KError, NonEmptyList[A]]
type HRIO[R, A] = ZIO[R, KError, A]
type HRION[R, A] = ZIO[R, KError, NonEmptyList[A]]
type SHTask[A] = ZIO[HarnessEnv, KError, A]
type SHTaskN[A] = ZIO[HarnessEnv, KError, NonEmptyList[A]]
type SHRIO[R, A] = ZIO[HarnessEnv with R, KError, A]
type SHRION[R, A] = ZIO[HarnessEnv with R, KError, NonEmptyList[A]]
