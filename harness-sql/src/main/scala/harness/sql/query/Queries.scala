package harness.sql.query

import cats.syntax.option.*
import harness.sql.*
import harness.sql.errors.*
import harness.sql.typeclass.*
import zio.*

final class InsertQueryI[I] private[query] (query: Insert.Query[I]) {
  def apply(i: I): RIO[ConnectionFactory, Int] =
    ZIO.scoped {
      Utils.preparedStatement(query.query, (i, query.encoder, QueryInputMapper.id).some).flatMap { ps =>
        ZIO.attempt(ps.executeUpdate()).mapError(ErrorWithSql(query.query, _))
      }
    }
}
// final class InsertQueryIO[I, O] private[query] () { // TODO (KR) :
//   def apply(i: I): HRIO[ConnectionFactory, O] = ??? // TODO (KR) :
// }

final class SelectQueryO[O] private[query] (query: Select.Query[O]) {
  def apply(): QueryResult[O] = QueryResult.stream(query.query, None, query.decoder)
}
final class SelectQueryIO[I, O] private[query] (encoder: RowEncoder[I], query: Select.Query[O]) {
  def apply(i: I): QueryResult[O] = QueryResult.stream(query.query, (i, encoder, query.queryInputMapper).some, query.decoder)
}

final class UpdateQuery private[query] () // TODO (KR) :
final class UpdateQueryI[I] private[query] () // TODO (KR) :
final class UpdateQueryO[O] private[query] () // TODO (KR) :
final class UpdateQueryIO[I, O] private[query] () // TODO (KR) :

final class DeleteQuery private[query] () // TODO (KR) :
final class DeleteQueryI[I] private[query] () // TODO (KR) :
final class DeleteQueryO[O] private[query] () // TODO (KR) :
final class DeleteQueryIO[I, O] private[query] () // TODO (KR) :
