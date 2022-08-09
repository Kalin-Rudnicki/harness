package harness.zio

import cats.data.NonEmptyList
import harness.core.*
import zio.*

type HTask[A] = IO[HError, A]
type HTaskN[A] = IO[NonEmptyList[HError], A]
type HRIO[R, A] = ZIO[R, HError, A]
type HRION[R, A] = ZIO[R, NonEmptyList[HError], A]
type SHTask[A] = ZIO[HarnessEnv, HError, A]
type SHTaskN[A] = ZIO[HarnessEnv, NonEmptyList[HError], A]
type SHRIO[R, A] = ZIO[HarnessEnv & R, HError, A]
type SHRION[R, A] = ZIO[HarnessEnv & R, NonEmptyList[HError], A]
