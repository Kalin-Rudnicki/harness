package harness.core

trait Zip[In1, In2] {
  type Out

  def zip(in1: In1, in2: In2): Out
  def unzip(out: Out): (In1, In2)
}
object Zip extends ZipLowPriority1 {

  type Out[In1, In2, O] = Zip[In1, In2] { type Out = O }

  def apply[In1, In2](implicit zip: Zip[In1, In2]): Zip[In1, In2] = zip

  implicit def zipLeftId[_1]: Zip.Out[Unit, _1, _1] =
    new Zip[Unit, _1] {
      override type Out = _1
      override def zip(in1: Unit, in2: _1): _1 = in2
      override def unzip(out: _1): (Unit, _1) = ((), out)
    }

}

trait ZipLowPriority1 extends ZipLowPriority2 {

  implicit def zipRightId[_1]: Zip.Out[_1, Unit, _1] =
    new Zip[_1, Unit] {
      override type Out = _1
      override def zip(in1: _1, in2: Unit): _1 = in1
      override def unzip(out: _1): (_1, Unit) = (out, ())
    }

}

trait ZipLowPriority2 extends ZipLowPriority3 {

  implicit def zipTuples[_1 <: Tuple, _2 <: Tuple](implicit size: ValueOf[Tuple.Size[_1]]): Zip.Out[_1, _2, Tuple.Concat[_1, _2]] =
    new Zip[_1, _2] {
      override type Out = Tuple.Concat[_1, _2]
      override def zip(in1: _1, in2: _2): Tuple.Concat[_1, _2] = in1 ++ in2
      override def unzip(out: Tuple.Concat[_1, _2]): (_1, _2) = out.splitAt(size.value).asInstanceOf[(_1, _2)]
    }

}

trait ZipLowPriority3 extends ZipLowPriority4 {

  implicit def zipIdTuple[_1, _2 <: Tuple]: Zip.Out[_1, _2, _1 *: _2] =
    new Zip[_1, _2] {
      override type Out = _1 *: _2
      override def zip(in1: _1, in2: _2): _1 *: _2 = in1 *: in2
      override def unzip(out: _1 *: _2): (_1, _2) = (out.head, out.tail)
    }

  implicit def zipTupleId[_1 <: Tuple, _2]: Zip.Out[_1, _2, Tuple.Append[_1, _2]] =
    new Zip[_1, _2] {
      override type Out = Tuple.Append[_1, _2]
      override def zip(in1: _1, in2: _2): Tuple.Append[_1, _2] = in1 :* in2
      override def unzip(out: Tuple.Append[_1, _2]): (_1, _2) = (out.init, out.last).asInstanceOf[(_1, _2)]
    }

}

trait ZipLowPriority4 {

  implicit def zip2[_1, _2]: Zip.Out[_1, _2, (_1, _2)] =
    new Zip[_1, _2] {
      override type Out = (_1, _2)
      override def zip(in1: _1, in2: _2): Out = (in1, in2)
      override def unzip(out: Out): (_1, _2) = out
    }

}
