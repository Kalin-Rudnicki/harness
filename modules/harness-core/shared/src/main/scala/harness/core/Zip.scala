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

// =====| Multi-Zip |=====

trait Zip3[In1, In2, In3] {
  type Out

  def zip(in1: In1, in2: In2, in3: In3): Out
  def unzip(out: Out): (In1, In2, In3)
}
object Zip3 {

  type Out[In1, In2, In3, O] = Zip3[In1, In2, In3] { type Out = O }

  def apply[In1, In2, In3](implicit zip: Zip3[In1, In2, In3]): Zip3[In1, In2, In3] = zip

  implicit def make[In1, In2, O1, In3, O2](implicit
      z1: Zip.Out[In1, In2, O1],
      z2: Zip.Out[O1, In3, O2],
  ): Zip3.Out[In1, In2, In3, O2] =
    new Zip3[In1, In2, In3] {
      override type Out = O2

      override def zip(in1: In1, in2: In2, in3: In3): O2 =
        z2.zip(z1.zip(in1, in2), in3)

      override def unzip(out: O2): (In1, In2, In3) = {
        val (tmp, i3) = z2.unzip(out)
        val (i1, i2) = z1.unzip(tmp)
        (i1, i2, i3)
      }
    }

}

trait Zip4[In1, In2, In3, In4] {
  type Out

  def zip(in1: In1, in2: In2, in3: In3, in4: In4): Out
  def unzip(out: Out): (In1, In2, In3, In4)
}
object Zip4 {

  type Out[In1, In2, In3, In4, O] = Zip4[In1, In2, In3, In4] { type Out = O }

  def apply[In1, In2, In3, In4](implicit zip: Zip4[In1, In2, In3, In4]): Zip4[In1, In2, In3, In4] = zip

  implicit def make[In1, In2, In3, O1, In4, O2](implicit
      z1: Zip3.Out[In1, In2, In3, O1],
      z2: Zip.Out[O1, In4, O2],
  ): Zip4.Out[In1, In2, In3, In4, O2] =
    new Zip4[In1, In2, In3, In4] {
      override type Out = O2

      override def zip(in1: In1, in2: In2, in3: In3, in4: In4): O2 =
        z2.zip(z1.zip(in1, in2, in3), in4)

      override def unzip(out: O2): (In1, In2, In3, In4) = {
        val (tmp, i4) = z2.unzip(out)
        val (i1, i2, i3) = z1.unzip(tmp)
        (i1, i2, i3, i4)
      }
    }

}

trait Zip5[In1, In2, In3, In4, In5] {
  type Out

  def zip(in1: In1, in2: In2, in3: In3, in4: In4, in5: In5): Out
  def unzip(out: Out): (In1, In2, In3, In4, In5)
}
object Zip5 {

  type Out[In1, In2, In3, In4, In5, O] = Zip5[In1, In2, In3, In4, In5] { type Out = O }

  def apply[In1, In2, In3, In4, In5](implicit zip: Zip5[In1, In2, In3, In4, In5]): Zip5[In1, In2, In3, In4, In5] = zip

  implicit def make[In1, In2, In3, In4, O1, In5, O2](implicit
      z1: Zip4.Out[In1, In2, In3, In4, O1],
      z2: Zip.Out[O1, In5, O2],
  ): Zip5.Out[In1, In2, In3, In4, In5, O2] =
    new Zip5[In1, In2, In3, In4, In5] {
      override type Out = O2

      override def zip(in1: In1, in2: In2, in3: In3, in4: In4, in5: In5): O2 =
        z2.zip(z1.zip(in1, in2, in3, in4), in5)

      override def unzip(out: O2): (In1, In2, In3, In4, In5) = {
        val (tmp, i5) = z2.unzip(out)
        val (i1, i2, i3, i4) = z1.unzip(tmp)
        (i1, i2, i3, i4, i5)
      }
    }

}

trait Zip6[In1, In2, In3, In4, In5, In6] {
  type Out

  def zip(in1: In1, in2: In2, in3: In3, in4: In4, in5: In5, in6: In6): Out
  def unzip(out: Out): (In1, In2, In3, In4, In5, In6)
}
object Zip6 {

  type Out[In1, In2, In3, In4, In5, In6, O] = Zip6[In1, In2, In3, In4, In5, In6] { type Out = O }

  def apply[In1, In2, In3, In4, In5, In6](implicit zip: Zip6[In1, In2, In3, In4, In5, In6]): Zip6[In1, In2, In3, In4, In5, In6] = zip

  implicit def make[In1, In2, In3, In4, In5, O1, In6, O2](implicit
      z1: Zip5.Out[In1, In2, In3, In4, In5, O1],
      z2: Zip.Out[O1, In6, O2],
  ): Zip6.Out[In1, In2, In3, In4, In5, In6, O2] =
    new Zip6[In1, In2, In3, In4, In5, In6] {
      override type Out = O2

      override def zip(in1: In1, in2: In2, in3: In3, in4: In4, in5: In5, in6: In6): O2 =
        z2.zip(z1.zip(in1, in2, in3, in4, in5), in6)

      override def unzip(out: O2): (In1, In2, In3, In4, In5, In6) = {
        val (tmp, i6) = z2.unzip(out)
        val (i1, i2, i3, i4, i5) = z1.unzip(tmp)
        (i1, i2, i3, i4, i5, i6)
      }
    }

}

trait Zip7[In1, In2, In3, In4, In5, In6, In7] {
  type Out

  def zip(in1: In1, in2: In2, in3: In3, in4: In4, in5: In5, in6: In6, in7: In7): Out
  def unzip(out: Out): (In1, In2, In3, In4, In5, In6, In7)
}
object Zip7 {

  type Out[In1, In2, In3, In4, In5, In6, In7, O] = Zip7[In1, In2, In3, In4, In5, In6, In7] { type Out = O }

  def apply[In1, In2, In3, In4, In5, In6, In7](implicit zip: Zip7[In1, In2, In3, In4, In5, In6, In7]): Zip7[In1, In2, In3, In4, In5, In6, In7] = zip

  implicit def make[In1, In2, In3, In4, In5, In6, O1, In7, O2](implicit
      z1: Zip6.Out[In1, In2, In3, In4, In5, In6, O1],
      z2: Zip.Out[O1, In7, O2],
  ): Zip7.Out[In1, In2, In3, In4, In5, In6, In7, O2] =
    new Zip7[In1, In2, In3, In4, In5, In6, In7] {
      override type Out = O2

      override def zip(in1: In1, in2: In2, in3: In3, in4: In4, in5: In5, in6: In6, in7: In7): O2 =
        z2.zip(z1.zip(in1, in2, in3, in4, in5, in6), in7)

      override def unzip(out: O2): (In1, In2, In3, In4, In5, In6, In7) = {
        val (tmp, i7) = z2.unzip(out)
        val (i1, i2, i3, i4, i5, i6) = z1.unzip(tmp)
        (i1, i2, i3, i4, i5, i6, i7)
      }
    }

}

trait Zip8[In1, In2, In3, In4, In5, In6, In7, In8] {
  type Out

  def zip(in1: In1, in2: In2, in3: In3, in4: In4, in5: In5, in6: In6, in7: In7, in8: In8): Out
  def unzip(out: Out): (In1, In2, In3, In4, In5, In6, In7, In8)
}
object Zip8 {

  type Out[In1, In2, In3, In4, In5, In6, In7, In8, O] = Zip8[In1, In2, In3, In4, In5, In6, In7, In8] { type Out = O }

  def apply[In1, In2, In3, In4, In5, In6, In7, In8](implicit zip: Zip8[In1, In2, In3, In4, In5, In6, In7, In8]): Zip8[In1, In2, In3, In4, In5, In6, In7, In8] = zip

  implicit def make[In1, In2, In3, In4, In5, In6, In7, O1, In8, O2](implicit
      z1: Zip7.Out[In1, In2, In3, In4, In5, In6, In7, O1],
      z2: Zip.Out[O1, In8, O2],
  ): Zip8.Out[In1, In2, In3, In4, In5, In6, In7, In8, O2] =
    new Zip8[In1, In2, In3, In4, In5, In6, In7, In8] {
      override type Out = O2

      override def zip(in1: In1, in2: In2, in3: In3, in4: In4, in5: In5, in6: In6, in7: In7, in8: In8): O2 =
        z2.zip(z1.zip(in1, in2, in3, in4, in5, in6, in7), in8)

      override def unzip(out: O2): (In1, In2, In3, In4, In5, In6, In7, In8) = {
        val (tmp, i8) = z2.unzip(out)
        val (i1, i2, i3, i4, i5, i6, i7) = z1.unzip(tmp)
        (i1, i2, i3, i4, i5, i6, i7, i8)
      }
    }

}

trait Zip9[In1, In2, In3, In4, In5, In6, In7, In8, In9] {
  type Out

  def zip(in1: In1, in2: In2, in3: In3, in4: In4, in5: In5, in6: In6, in7: In7, in8: In8, in9: In9): Out
  def unzip(out: Out): (In1, In2, In3, In4, In5, In6, In7, In8, In9)
}
object Zip9 {

  type Out[In1, In2, In3, In4, In5, In6, In7, In8, In9, O] = Zip9[In1, In2, In3, In4, In5, In6, In7, In8, In9] { type Out = O }

  def apply[In1, In2, In3, In4, In5, In6, In7, In8, In9](implicit zip: Zip9[In1, In2, In3, In4, In5, In6, In7, In8, In9]): Zip9[In1, In2, In3, In4, In5, In6, In7, In8, In9] = zip

  implicit def make[In1, In2, In3, In4, In5, In6, In7, In8, O1, In9, O2](implicit
      z1: Zip8.Out[In1, In2, In3, In4, In5, In6, In7, In8, O1],
      z2: Zip.Out[O1, In9, O2],
  ): Zip9.Out[In1, In2, In3, In4, In5, In6, In7, In8, In9, O2] =
    new Zip9[In1, In2, In3, In4, In5, In6, In7, In8, In9] {
      override type Out = O2

      override def zip(in1: In1, in2: In2, in3: In3, in4: In4, in5: In5, in6: In6, in7: In7, in8: In8, in9: In9): O2 =
        z2.zip(z1.zip(in1, in2, in3, in4, in5, in6, in7, in8), in9)

      override def unzip(out: O2): (In1, In2, In3, In4, In5, In6, In7, In8, In9) = {
        val (tmp, i9) = z2.unzip(out)
        val (i1, i2, i3, i4, i5, i6, i7, i8) = z1.unzip(tmp)
        (i1, i2, i3, i4, i5, i6, i7, i8, i9)
      }
    }

}

trait Zip10[In1, In2, In3, In4, In5, In6, In7, In8, In9, In10] {
  type Out

  def zip(in1: In1, in2: In2, in3: In3, in4: In4, in5: In5, in6: In6, in7: In7, in8: In8, in9: In9, in10: In10): Out
  def unzip(out: Out): (In1, In2, In3, In4, In5, In6, In7, In8, In9, In10)
}
object Zip10 {

  type Out[In1, In2, In3, In4, In5, In6, In7, In8, In9, In10, O] = Zip10[In1, In2, In3, In4, In5, In6, In7, In8, In9, In10] { type Out = O }

  def apply[In1, In2, In3, In4, In5, In6, In7, In8, In9, In10](implicit zip: Zip10[In1, In2, In3, In4, In5, In6, In7, In8, In9, In10]): Zip10[In1, In2, In3, In4, In5, In6, In7, In8, In9, In10] = zip

  implicit def make[In1, In2, In3, In4, In5, In6, In7, In8, In9, O1, In10, O2](implicit
      z1: Zip9.Out[In1, In2, In3, In4, In5, In6, In7, In8, In9, O1],
      z2: Zip.Out[O1, In10, O2],
  ): Zip10.Out[In1, In2, In3, In4, In5, In6, In7, In8, In9, In10, O2] =
    new Zip10[In1, In2, In3, In4, In5, In6, In7, In8, In9, In10] {
      override type Out = O2

      override def zip(in1: In1, in2: In2, in3: In3, in4: In4, in5: In5, in6: In6, in7: In7, in8: In8, in9: In9, in10: In10): O2 =
        z2.zip(z1.zip(in1, in2, in3, in4, in5, in6, in7, in8, in9), in10)

      override def unzip(out: O2): (In1, In2, In3, In4, In5, In6, In7, In8, In9, In10) = {
        val (tmp, i10) = z2.unzip(out)
        val (i1, i2, i3, i4, i5, i6, i7, i8, i9) = z1.unzip(tmp)
        (i1, i2, i3, i4, i5, i6, i7, i8, i9, i10)
      }
    }

}
