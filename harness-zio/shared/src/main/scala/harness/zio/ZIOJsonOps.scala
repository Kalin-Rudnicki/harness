package harness.zio

import harness.core.*
import scala.reflect.ClassTag
import zio.json.*

extension (self: JsonCodec.type) {

  def `enum`[E <: Enum[E], Enc](implicit ec: JsonCodec[Enc], ewe: Enum.WithEnc[E, Enc], ct: ClassTag[E]): JsonCodec[E] =
    ec.transformOrFail(
      e => ewe.decode(e).toRight(s"Invalid ${ct.runtimeClass.getSimpleName}: $e"),
      ewe.encode,
    )

}
