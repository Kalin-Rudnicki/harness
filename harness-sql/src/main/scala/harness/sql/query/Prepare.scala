package harness.sql.query

// TODO (KR) :
object Prepare {

  def insertO[I](query: Insert.Query[I]): InsertQueryI[I] = InsertQueryI(query)
  // def insertIO[I, O]: InsertQueryIO[I, O] = ??? // TODO (KR) :

  def selectO[O](query: Select.Query[O]): SelectQueryO[O] = SelectQueryO(query)
  def selectIO[I, Q, O](input: Input[I, Q])(f: Q => Select.Query[O]): SelectQueryIO[I, O] = SelectQueryIO(input.encoder, f(input.buildQ(0)))

  def update[T](query: Update.Query[T]): UpdateQuery = UpdateQuery(query.asInstanceOf)
  def updateI[I, Q, T](input: Input[I, Q])(f: Q => Update.Query[T]): UpdateQueryI[I] = UpdateQueryI(input.encoder, f(input.buildQ(0)).asInstanceOf)
  def updateO[O](query: Update.QueryR[O]): UpdateQueryO[O] = UpdateQueryO(query)
  def updateIO[I, Q, O](input: Input[I, Q])(f: Q => Update.QueryR[O]): UpdateQueryIO[I, O] = UpdateQueryIO(input.encoder, f(input.buildQ(0)).asInstanceOf)

  def delete[T](query: Delete.Query[T]): DeleteQuery = DeleteQuery(query.asInstanceOf)
  def deleteI[I, Q, T](input: Input[I, Q])(f: Q => Delete.Query[T]): DeleteQueryI[I] = DeleteQueryI(input.encoder, f(input.buildQ(0)).asInstanceOf)
  def deleteO[O](query: Delete.QueryR[O]): DeleteQueryO[O] = DeleteQueryO(query)
  def deleteIO[I, Q, O](input: Input[I, Q])(f: Q => Delete.QueryR[O]): DeleteQueryIO[I, O] = DeleteQueryIO(input.encoder, f(input.buildQ(0)).asInstanceOf)

}
