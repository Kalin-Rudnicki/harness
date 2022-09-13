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

final class UpdateQuery private[query] (query: Update.Query[Any]) {
  def apply(): RIO[ConnectionFactory, Int] =
    ZIO.scoped {
      Utils.preparedStatement(query.query, None).flatMap { ps =>
        ZIO.attempt(ps.executeUpdate()).mapError(ErrorWithSql(query.query, _))
      }
    }
}
final class UpdateQueryI[I] private[query] (encoder: RowEncoder[I], query: Update.Query[Any]) {
  def apply(i: I): RIO[ConnectionFactory, Int] =
    ZIO.scoped {
      Utils.preparedStatement(query.query, (i, encoder, query.queryInputMapper).some).flatMap { ps =>
        ZIO.attempt(ps.executeUpdate()).mapError(ErrorWithSql(query.query, _))
      }
    }
}
final class UpdateQueryO[O] private[query] (query: Update.QueryR[O]) {
  def apply(): QueryResult[O] = QueryResult.stream(query.query, None, query.decoder)
}
final class UpdateQueryIO[I, O] private[query] (encoder: RowEncoder[I], query: Update.QueryR[O]) {
  def apply(i: I): QueryResult[O] = QueryResult.stream(query.query, (i, encoder, query.queryInputMapper).some, query.decoder)
}

final class DeleteQuery private[query] (query: Delete.Query[Any]) {
  def apply(): RIO[ConnectionFactory, Int] =
    ZIO.scoped {
      Utils.preparedStatement(query.query, None).flatMap { ps =>
        ZIO.attempt(ps.executeUpdate()).mapError(ErrorWithSql(query.query, _))
      }
    }
}
final class DeleteQueryI[I] private[query] (encoder: RowEncoder[I], query: Delete.Query[Any]) {
  def apply(i: I): RIO[ConnectionFactory, Int] =
    ZIO.scoped {
      Utils.preparedStatement(query.query, (i, encoder, query.queryInputMapper).some).flatMap { ps =>
        ZIO.attempt(ps.executeUpdate()).mapError(ErrorWithSql(query.query, _))
      }
    }
}
final class DeleteQueryO[O] private[query] (query: Delete.QueryR[O]) {
  def apply(): QueryResult[O] = QueryResult.stream(query.query, None, query.decoder)
}
final class DeleteQueryIO[I, O] private[query] (encoder: RowEncoder[I], query: Delete.QueryR[O]) {
  def apply(i: I): QueryResult[O] = QueryResult.stream(query.query, (i, encoder, query.queryInputMapper).some, query.decoder)
}
