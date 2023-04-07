package harness.sql.query

import harness.sql.*
import harness.zio.*
import zio.*

trait Transaction {
  def inTransaction[R <: JDBCConnection & Logger & Telemetry, A](effect: HRIO[R, A]): HRIO[R, A]
  def inSavepoint[R <: JDBCConnection & Logger & Telemetry, A](effect: HRIO[R, A]): HRIO[R, A]
}
object Transaction {

  def inTransaction[R <: JDBCConnection & Logger & Telemetry, A](effect: HRIO[R, A]): HRIO[Transaction & R, A] =
    ZIO.serviceWithZIO[Transaction](_.inTransaction(effect))

  // NOTE : This should only be run inside a transaction
  def inSavepoint[R <: JDBCConnection & Logger & Telemetry, A](effect: HRIO[R, A]): HRIO[Transaction & R, A] =
    ZIO.serviceWithZIO[Transaction](_.inSavepoint(effect))

  // =====|  |=====

  final class Savepoint private (override val toString: String)
  object Savepoint {

    val gen: UIO[Savepoint] =
      Random.nextUUID.map { uuid => Savepoint(s"savepoint_${uuid.toString.replace("-", "")}") }

  }

  object raw {

    val begin: Query = Query("BEGIN", fr"BEGIN")

    val commit: Query = Query("COMMIT", fr"COMMIT")

    val rollback: Query = Query("ROLLBACK", fr"ROLLBACK")

    def savepoint(savepoint: Savepoint): Query = Query("SAVEPOINT", fr"SAVEPOINT ${savepoint.toString}")

    def releaseSavepoint(savepoint: Savepoint): Query = Query("RELEASE SAVEPOINT", fr"RELEASE SAVEPOINT ${savepoint.toString}")

    def rollbackSavepoint(savepoint: Savepoint): Query = Query("ROLLBACK TO SAVEPOINT", fr"ROLLBACK TO SAVEPOINT ${savepoint.toString}")

  }

  // =====|  |=====

  object Live extends Transaction {

    private def genericTransaction[R <: JDBCConnection & Logger & Telemetry, A](begin: Query, rollback: Query, commit: Query)(effect: HRIO[R, A]): HRIO[R, A] =
      (
        begin().unit *>
          effect.interruptible.foldCauseZIO(
            e =>
              rollback().unit
                .foldCauseZIO(
                  e2 => ZIO.failCause(Cause.Then(e, e2)),
                  _ => ZIO.failCause(e),
                ),
            a => commit().unit.as(a),
          )
      ).uninterruptible

    override def inTransaction[R <: JDBCConnection & Logger & Telemetry, A](effect: HRIO[R, A]): HRIO[R, A] =
      genericTransaction(
        begin = Transaction.raw.begin,
        rollback = Transaction.raw.rollback,
        commit = Transaction.raw.commit,
      )(effect)

    override def inSavepoint[R <: JDBCConnection & Logger & Telemetry, A](effect: HRIO[R, A]): HRIO[R, A] =
      Savepoint.gen.flatMap { savepoint =>
        genericTransaction(
          begin = Transaction.raw.savepoint(savepoint),
          rollback = Transaction.raw.rollbackSavepoint(savepoint),
          commit = Transaction.raw.releaseSavepoint(savepoint),
        )(effect)
      }

  }

  object UseSavepointForTransaction extends Transaction {
    override def inTransaction[R <: JDBCConnection & Logger & Telemetry, A](effect: HRIO[R, A]): HRIO[R, A] = Live.inSavepoint(effect)
    override def inSavepoint[R <: JDBCConnection & Logger & Telemetry, A](effect: HRIO[R, A]): HRIO[R, A] = Live.inSavepoint(effect)
  }

  object Transactionless extends Transaction {
    override def inTransaction[R <: JDBCConnection & Logger & Telemetry, A](effect: HRIO[R, A]): HRIO[R, A] = effect
    override def inSavepoint[R <: JDBCConnection & Logger & Telemetry, A](effect: HRIO[R, A]): HRIO[R, A] = effect
  }

}
