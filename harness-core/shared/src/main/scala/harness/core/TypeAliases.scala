package harness.core

import cats.data.EitherNel

type EitherError[A] = Either[KError, A]
type EitherErrorNel[A] = EitherNel[KError, A]
