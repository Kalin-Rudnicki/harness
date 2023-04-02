package harness.sql.query

object Prepare {

  def insertO[I](queryName: String)(query: Insert.Query[I]): QueryI[I] =
    QueryI(queryName, query.query, query.encoder, QueryInputMapper.id)
  // def insertIO[I, O]: InsertQueryIO[I, O] = ??? // TODO (KR) :

  def selectO[O](queryName: String)(query: Select.Query[O]): QueryO[O] =
    QueryO(queryName, query.query, query.queryInputMapper, query.decoder)
  def selectIO[I, Q, O](queryName: String)(input: Input[I, Q])(f: Q => Select.Query[O]): QueryIO[I, O] = {
    val q = f(input.buildQ(0))
    QueryIO(queryName, q.query, input.encoder, q.queryInputMapper, q.decoder)
  }

  def update[T](queryName: String)(query: Update.Query[T]): Query =
    Query(queryName, query.query, query.queryInputMapper)
  def updateI[I, Q, T](queryName: String)(input: Input[I, Q])(f: Q => Update.Query[T]): QueryI[I] = {
    val q = f(input.buildQ(0))
    QueryI(queryName, q.query, input.encoder, q.queryInputMapper)
  }
  def updateO[O](queryName: String)(query: Update.QueryR[O]): QueryO[O] =
    QueryO(queryName, query.query, query.queryInputMapper, query.decoder)
  def updateIO[I, Q, O](queryName: String)(input: Input[I, Q])(f: Q => Update.QueryR[O]): QueryIO[I, O] = {
    val q = f(input.buildQ(0))
    QueryIO(queryName, q.query, input.encoder, q.queryInputMapper, q.decoder)
  }

  def delete[T](queryName: String)(query: Delete.Query[T]): Query =
    Query(queryName, query.query, query.queryInputMapper)
  def deleteI[I, Q, T](queryName: String)(input: Input[I, Q])(f: Q => Delete.Query[T]): QueryI[I] = {
    val q = f(input.buildQ(0))
    QueryI(queryName, q.query, input.encoder, q.queryInputMapper)
  }
  def deleteO[O](queryName: String)(query: Delete.QueryR[O]): QueryO[O] =
    QueryO(queryName, query.query, query.queryInputMapper, query.decoder)
  def deleteIO[I, Q, O](queryName: String)(input: Input[I, Q])(f: Q => Delete.QueryR[O]): QueryIO[I, O] = {
    val q = f(input.buildQ(0))
    QueryIO(queryName, q.query, input.encoder, q.queryInputMapper, q.decoder)
  }

}
