package harness.sql.query

// TODO (KR) :
object Prepare {

  def insertO[I](query: Insert.Query[I]): InsertQueryI[I] = InsertQueryI(query)
  // def insertIO[I, O]: InsertQueryIO[I, O] = ??? // TODO (KR) :

  def selectO[O](query: Select.Query[O]): SelectQueryO[O] = SelectQueryO(query)
  def selectIO[I, Q, O](input: Input[I, Q])(f: Q => Select.Query[O]): SelectQueryIO[I, O] = SelectQueryIO(input.encoder, f(input.buildQ(0)))

  def update: UpdateQuery = ??? // TODO (KR) :
  def updateI[I, Q]: UpdateQueryI[I] = ??? // TODO (KR) :
  def updateO[O]: UpdateQueryO[O] = ??? // TODO (KR) :
  def updateIO[I, Q, O]: UpdateQueryIO[I, O] = ??? // TODO (KR) :

  def delete: DeleteQuery = ??? // TODO (KR) :
  def deleteI[I, Q]: DeleteQueryI[I] = ??? // TODO (KR) :
  def deleteO[O]: DeleteQueryO[O] = ??? // TODO (KR) :
  def deleteIO[I, Q, O]: DeleteQueryIO[I, O] = ??? // TODO (KR) :

}
