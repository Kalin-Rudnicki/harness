package harness.core

object EitherN {

  sealed trait Either10[+A, +B, +C, +D, +E, +F, +G, +H, +I, +J]
  final case class _10[+T](value: T) extends Either10[Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, T]

  sealed trait Either9[+A, +B, +C, +D, +E, +F, +G, +H, +I] extends Either10[A, B, C, D, E, F, G, H, I, Nothing]
  final case class _9[+T](value: T) extends Either9[Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, T]

  sealed trait Either8[+A, +B, +C, +D, +E, +F, +G, +H] extends Either9[A, B, C, D, E, F, G, H, Nothing]
  final case class _8[+T](value: T) extends Either8[Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, T]

  sealed trait Either7[+A, +B, +C, +D, +E, +F, +G] extends Either8[A, B, C, D, E, F, G, Nothing]
  final case class _7[+T](value: T) extends Either7[Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, T]

  sealed trait Either6[+A, +B, +C, +D, +E, +F] extends Either7[A, B, C, D, E, F, Nothing]
  final case class _6[+T](value: T) extends Either6[Nothing, Nothing, Nothing, Nothing, Nothing, T]

  sealed trait Either5[+A, +B, +C, +D, +E] extends Either6[A, B, C, D, E, Nothing]
  final case class _5[+T](value: T) extends Either5[Nothing, Nothing, Nothing, Nothing, T]

  sealed trait Either4[+A, +B, +C, +D] extends Either5[A, B, C, D, Nothing]
  final case class _4[+T](value: T) extends Either4[Nothing, Nothing, Nothing, T]

  sealed trait Either3[+A, +B, +C] extends Either4[A, B, C, Nothing]
  final case class _3[+T](value: T) extends Either3[Nothing, Nothing, T]

  sealed trait Either2[+A, +B] extends Either3[A, B, Nothing]
  final case class _2[+T](value: T) extends Either2[Nothing, T]
  final case class _1[+T](value: T) extends Either2[T, Nothing]

}
