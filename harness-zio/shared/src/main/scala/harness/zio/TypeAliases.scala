package harness.zio

import cats.data.NonEmptyList
import harness.core.*
import zio.*

type HTask[A] = IO[KError, A]
type HTaskN[A] = IO[NonEmptyList[KError], A]
type HRIO[R, A] = ZIO[R, KError, A]
type HRION[R, A] = ZIO[R, NonEmptyList[KError], A]
type SHTask[A] = ZIO[HarnessEnv, KError, A]
type SHTaskN[A] = ZIO[HarnessEnv, NonEmptyList[KError], A]
type SHRIO[R, A] = ZIO[HarnessEnv & R, KError, A]
type SHRION[R, A] = ZIO[HarnessEnv & R, NonEmptyList[KError], A]
