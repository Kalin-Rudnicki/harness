package harness.sql.query

import harness.core.Zip
import zio.Chunk

final class Input[I, V] private (
    private val width: Int,
    private val _buildInputVars: Int => V,
    private[sql] val buildUnmappedInputs: I => Chunk[Any],
) { self =>

  def ~[I2, V2](other: Input[I2, V2])(implicit zI: Zip[I, I2], zV: Zip[V, V2]): Input[zI.Out, zV.Out] =
    new Input[zI.Out, zV.Out](
      self.width + other.width,
      o => zV.zip(self._buildInputVars(o), other._buildInputVars(o + self.width)),
      { i =>
        val (a, b) = zI.unzip(i)
        self.buildUnmappedInputs(a) ++ other.buildUnmappedInputs(b)
      },
    )

  def cmap[I2](f: I2 => I): Input[I2, V] =
    new Input[I2, V](
      width,
      _buildInputVars,
      i => buildUnmappedInputs(f(i)),
    )

  private[query] def buildInputVars: V = _buildInputVars(0)

}
object Input {

  def apply[T]: Input[T, QueryInputVar[T]] =
    new Input[T, QueryInputVar[T]](
      1,
      QueryInputVar[T],
      Chunk.single[T],
    )

}
