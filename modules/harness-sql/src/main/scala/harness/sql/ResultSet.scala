package harness.sql

import harness.sql.error.QueryError
import harness.sql.query.Fragment
import harness.sql.typeclass.QueryDecoderMany
import zio.*

final class ResultSet[O](
    queryName: String,
    fragment: Fragment,
    rs: java.sql.ResultSet,
    decoder: QueryDecoderMany[O],
) {

  private inline def attemptGeneric[A](hint: String)(thunk: => A): IO[QueryError, A] =
    QueryError.attempt(queryName, fragment.sql) { thunk }(QueryError.Cause.Generic(hint, _))

  private inline def getObj(k: Option[Class[?]], i: Int): Object =
    k match {
      case Some(k) => rs.getObject(i + 1, k)
      case None    => rs.getObject(i + 1)
    }

  private val decodeResult: IO[QueryError, O] =
    for {
      ncs <- attemptGeneric("Unable to get result width") { rs.getMetaData.getColumnCount }
      outputs <-
        if (ncs == decoder.width) attemptGeneric("Unable to get result objects") { decoder.classes.zipWithIndex.map(getObj(_, _)) }
        else ZIO.fail(QueryError(queryName, fragment.sql, QueryError.Cause.InvalidResultSetWidth(decoder.width, ncs)))
      res <- attemptGeneric("Unable to decode row") { decoder.decodeMany(0, outputs) }.flatMap {
        case Right(value) => ZIO.succeed(value)
        case Left(errors) => ZIO.fail(QueryError(queryName, fragment.sql, QueryError.Cause.RowDecodeFailure(errors, outputs)))
      }
    } yield res

  val decodeRowOpt: IO[Option[QueryError], O] =
    attemptGeneric("Unable to get query next") { rs.next() }.asSomeError.flatMap {
      case true  => decodeResult.asSomeError
      case false => ZIO.fail(None)
    }

  // TODO (KR) :

}
