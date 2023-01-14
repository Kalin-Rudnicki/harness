package harness.zio

import cats.data.NonEmptyList
import cats.syntax.either.*
import zio.json.*

object ZIOJsonInstances {

  implicit def catsNelJsonCodec[A: JsonCodec]: JsonCodec[NonEmptyList[A]] =
    JsonCodec
      .list[A]
      .transformOrFail(
        {
          case head :: tail => NonEmptyList(head, tail).asRight
          case Nil          => "NonEmptyList has 0 elements".asLeft
        },
        _.toList,
      )

}
