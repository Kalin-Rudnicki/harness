package harness.sql.query

object Prepare {

  def insertO[I](query: Insert.Query[I]): QueryI[I] =
    QueryI(query.query, query.encoder, QueryInputMapper.id)
  // def insertIO[I, O]: InsertQueryIO[I, O] = ??? // TODO (KR) :

  def selectO[O](query: Select.Query[O]): QueryO[O] =
    QueryO(query.query, query.queryInputMapper, query.decoder)
  def selectIO[I, Q, O](input: Input[I, Q])(f: Q => Select.Query[O]): QueryIO[I, O] = {
    val q = f(input.buildQ(0))
    QueryIO(q.query, input.encoder, q.queryInputMapper, q.decoder)
  }

  def update[T](query: Update.Query[T]): Query =
    Query(query.query, query.queryInputMapper)
  def updateI[I, Q, T](input: Input[I, Q])(f: Q => Update.Query[T]): QueryI[I] = {
    val q = f(input.buildQ(0))
    QueryI(q.query, input.encoder, q.queryInputMapper)
  }
  def updateO[O](query: Update.QueryR[O]): QueryO[O] =
    QueryO(query.query, query.queryInputMapper, query.decoder)
  def updateIO[I, Q, O](input: Input[I, Q])(f: Q => Update.QueryR[O]): QueryIO[I, O] = {
    val q = f(input.buildQ(0))
    QueryIO(q.query, input.encoder, q.queryInputMapper, q.decoder)
  }

  def delete[T](query: Delete.Query[T]): Query =
    Query(query.query, query.queryInputMapper)
  def deleteI[I, Q, T](input: Input[I, Q])(f: Q => Delete.Query[T]): QueryI[I] = {
    val q = f(input.buildQ(0))
    QueryI(q.query, input.encoder, q.queryInputMapper)
  }
  def deleteO[O](query: Delete.QueryR[O]): QueryO[O] =
    QueryO(query.query, query.queryInputMapper, query.decoder)
  def deleteIO[I, Q, O](input: Input[I, Q])(f: Q => Delete.QueryR[O]): QueryIO[I, O] = {
    val q = f(input.buildQ(0))
    QueryIO(q.query, input.encoder, q.queryInputMapper, q.decoder)
  }

}
