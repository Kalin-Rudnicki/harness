package harness.core

import cats.data.EitherNel

type EitherError[A] = Either[HError, A]
type EitherErrorNel[A] = EitherNel[HError, A]
