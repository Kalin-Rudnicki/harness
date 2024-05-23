package harness.sql.query

import harness.sql.typeclass.QueryEncoderMany
import zio.Chunk

trait QueryInputMapper { self =>

  private[sql] def prepare(unmappedInputs: Chunk[Any]): Chunk[EncodedInputValue]

  final def +(other: QueryInputMapper): QueryInputMapper =
    (self, other) match {
      case (QueryInputMapper.Empty, other)                                             => other
      case (self, QueryInputMapper.Empty)                                              => self
      case (QueryInputMapper.Const(encodedSelf), QueryInputMapper.Const(encodedOther)) => QueryInputMapper.Const(encodedSelf ++ encodedOther)
      case (self, other) =>
        new QueryInputMapper {
          override private[sql] def prepare(unmappedInputs: Chunk[Any]) =
            self.prepare(unmappedInputs) ++ other.prepare(unmappedInputs)
        }
    }

}
object QueryInputMapper {

  final case class Const(encodedValues: Chunk[EncodedInputValue]) extends QueryInputMapper {
    override private[sql] def prepare(unmappedInputs: Chunk[Any]): Chunk[EncodedInputValue] = encodedValues
  }

  case object Empty extends QueryInputMapper {
    override private[sql] def prepare(unmappedInputs: Chunk[Any]): Chunk[EncodedInputValue] = Chunk.empty
  }

  def materialize[T](constant: Constant[T], encoder: QueryEncoderMany[T]): QueryInputMapper =
    QueryInputMapper.Const { encoder.encodeMany(constant.value).zipWith(encoder.klasses)(EncodedInputValue(_, _)) }

  def single[T](get: Chunk[Any] => T, encoder: QueryEncoderMany[T]): QueryInputMapper =
    new QueryInputMapper {
      override private[sql] def prepare(unmappedInputs: Chunk[Any]): Chunk[EncodedInputValue] = {
        val input = get(unmappedInputs)
        encoder.encodeMany(input).zipWith(encoder.klasses)(EncodedInputValue(_, _))
      }
    }

}
