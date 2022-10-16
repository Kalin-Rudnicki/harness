package harness.sql.query

import harness.core.Zip
import harness.sql.typeclass.RowEncoder

final class Input[I, Q] private (
    private[sql] val encoder: RowEncoder[I],
    private[query] val buildQ: Int => Q,
) { self =>

  def ~[I2, O2](other: Input[I2, O2])(implicit zI: Zip[I, I2], zQ: Zip[Q, O2]): Input[zI.Out, zQ.Out] =
    new Input[zI.Out, zQ.Out](
      self.encoder ~ other.encoder,
      i => zQ.zip(self.buildQ(i), other.buildQ(i + self.encoder.width)),
    )

}
object Input {

  def apply[T]: Input[T, QueryInput[T]] =
    new Input[T, QueryInput[T]](
      new RowEncoder[T] {
        override lazy val width: Int = 1
        override def encodeRow(t: T, o: Int, arr: Array[Object]): Unit = arr(o) = t.asInstanceOf[Object]
      },
      QueryInput[T],
    )

}
