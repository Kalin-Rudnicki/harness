package harness.sql.query

import cats.data.EitherNel
import cats.syntax.option.*
import harness.sql.*
import harness.sql.typeclass.*
import zio.*

final case class Query(queryName: String, fragment: Fragment) {

  def apply(): IntQueryResult =
    IntQueryResult(
      queryName,
      fragment,
      ZIO.scoped {
        PreparedStatement
          .make(queryName, fragment)
          .flatMap { _.executeUpdate }
      },
    )

}

final case class QueryI[I](queryName: String, fragment: Fragment, input: Input[I, ?]) { self =>

  def apply(i: I): IntQueryResult =
    IntQueryResult(
      queryName,
      fragment,
      ZIO.scoped {
        PreparedStatement
          .make(queryName, fragment)
          .tap { _.writeSingle(i, input) }
          .flatMap { _.executeUpdate }
      },
    )
  def batched(is: Chunk[I]): BatchQueryResult =
    BatchQueryResult(
      queryName,
      fragment,
      ZIO.scoped {
        PreparedStatement
          .make(queryName, fragment)
          .tap { _.writeBatched(is, input) }
          .flatMap { _.executeBatch }
      },
    )

  def apply[I1, I2](i1: I1, i2: I2)(implicit ev: (I1, I2) <:< I): IntQueryResult =
    self(ev((i1, i2)))
  def apply[I1, I2, I3](i1: I1, i2: I2, i3: I3)(implicit ev: (I1, I2, I3) <:< I): IntQueryResult =
    self(ev((i1, i2, i3)))
  def apply[I1, I2, I3, I4](i1: I1, i2: I2, i3: I3, i4: I4)(implicit ev: (I1, I2, I3, I4) <:< I): IntQueryResult =
    self(ev((i1, i2, i3, i4)))
  def apply[I1, I2, I3, I4, I5](i1: I1, i2: I2, i3: I3, i4: I4, i5: I5)(implicit ev: (I1, I2, I3, I4, I5) <:< I): IntQueryResult =
    self(ev((i1, i2, i3, i4, i5)))

  def cmap[I2](f: I2 => I): QueryI[I2] = QueryI(queryName, fragment, input.cmap(f))

}

final case class QueryO[O](queryName: String, fragment: Fragment, decoder: QueryDecoderMany[O]) {

  def apply(): QueryResult[O] = QueryResult.stream(queryName, fragment, None, decoder)

  def map[O2](f: O => O2): QueryO[O2] = QueryO(queryName, fragment, decoder.map(f))
  def emap[O2](f: O => EitherNel[String, O2]): QueryO[O2] = QueryO(queryName, fragment, decoder.emap(f))

  def ignoreOutput: Query = Query(queryName, fragment)

}

final case class QueryIO[I, O](queryName: String, fragment: Fragment, input: Input[I, ?], decoder: QueryDecoderMany[O]) { self =>

  def apply(i: I): QueryResult[O] = QueryResult.stream(queryName, fragment, (i, input).some, decoder)

  def apply[I1, I2](i1: I1, i2: I2)(implicit ev: (I1, I2) <:< I): QueryResult[O] =
    self(ev((i1, i2)))
  def apply[I1, I2, I3](i1: I1, i2: I2, i3: I3)(implicit ev: (I1, I2, I3) <:< I): QueryResult[O] =
    self(ev((i1, i2, i3)))
  def apply[I1, I2, I3, I4](i1: I1, i2: I2, i3: I3, i4: I4)(implicit ev: (I1, I2, I3, I4) <:< I): QueryResult[O] =
    self(ev((i1, i2, i3, i4)))
  def apply[I1, I2, I3, I4, I5](i1: I1, i2: I2, i3: I3, i4: I4, i5: I5)(implicit ev: (I1, I2, I3, I4, I5) <:< I): QueryResult[O] =
    self(ev((i1, i2, i3, i4, i5)))

  def cmap[I2](f: I2 => I): QueryIO[I2, O] = QueryIO(queryName, fragment, input.cmap(f), decoder)
  def map[O2](f: O => O2): QueryIO[I, O2] = QueryIO(queryName, fragment, input, decoder.map(f))
  def emap[O2](f: O => EitherNel[String, O2]): QueryIO[I, O2] = QueryIO(queryName, fragment, input, decoder.emap(f))

  def ignoreOutput: QueryI[I] = QueryI(queryName, fragment, input)

}
