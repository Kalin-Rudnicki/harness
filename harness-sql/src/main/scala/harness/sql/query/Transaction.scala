package harness.sql.query

import harness.sql.*
import harness.zio.*
import zio.*

object Transaction {

  final class Savepoint private (override val toString: String)
  object Savepoint {

    val gen: UIO[Savepoint] =
      Random.nextUUID.map { uuid => Savepoint(s"savepoint_${uuid.toString.replace("-", "")}") }

  }

  object raw {

    val begin: Query = Query("BEGIN", QueryInputMapper.empty)

    val commit: Query = Query("COMMIT", QueryInputMapper.empty)

    val rollback: Query = Query("ROLLBACK", QueryInputMapper.empty)

    def savepoint(savepoint: Savepoint): Query = Query(s"SAVEPOINT $savepoint", QueryInputMapper.empty)

    def releaseSavepoint(savepoint: Savepoint): Query = Query(s"RELEASE SAVEPOINT $savepoint", QueryInputMapper.empty)

    def rollbackSavepoint(savepoint: Savepoint): Query = Query(s"ROLLBACK TO SAVEPOINT $savepoint", QueryInputMapper.empty)

  }

  private def genericTransaction[R <: JDBCConnection, A](begin: Query, rollback: Query, commit: Query)(effect: HRIO[R, A]): HRIO[R, A] =
    (
      begin() *>
        effect.interruptible.foldCauseZIO(
          e =>
            rollback()
              .foldCauseZIO(
                e2 => ZIO.failCause(Cause.Then(e, e2)),
                _ => ZIO.failCause(e),
              ),
          a => commit().as(a),
        )
    ).uninterruptible

  def inTransaction[R <: JDBCConnection, A](effect: HRIO[R, A]): HRIO[R, A] =
    genericTransaction(
      begin = Transaction.raw.begin,
      rollback = Transaction.raw.rollback,
      commit = Transaction.raw.commit,
    )(effect)

  // NOTE : This should only be run inside a transaction
  def inSavepoint[R <: JDBCConnection, A](effect: HRIO[R, A]): HRIO[R, A] =
    Savepoint.gen.flatMap { savepoint =>
      genericTransaction(
        begin = Transaction.raw.savepoint(savepoint),
        rollback = Transaction.raw.rollbackSavepoint(savepoint),
        commit = Transaction.raw.releaseSavepoint(savepoint),
      )(effect)
    }

}
