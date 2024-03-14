package harness.sql.query

import harness.sql.*
import harness.sql.error.QueryError
import harness.zio.*
import zio.*

trait Transaction[E] { self =>

  def inTransaction[R <: Logger & Telemetry, A](effect: ZIO[R, E, A]): ZIO[R, E, A]

  def inSavepoint[R <: Logger & Telemetry, A](effect: ZIO[R, E, A]): ZIO[R, E, A]

}
object Transaction {

  def inTransaction[R <: Logger & Telemetry, E: Tag, A](effect: ZIO[R, E, A]): ZIO[Transaction[E] & R, E, A] =
    ZIO.serviceWithZIO[Transaction[E]](_.inTransaction(effect))

  // NOTE : This should only be run inside a transaction
  def inSavepoint[R <: Logger & Telemetry, E: Tag, A](effect: ZIO[R, E, A]): ZIO[Transaction[E] & R, E, A] =
    ZIO.serviceWithZIO[Transaction[E]](_.inSavepoint(effect))

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

  private object Helpers {

    def genericTransaction[R <: JDBCConnection & Logger & Telemetry, E, A](
        begin: Query,
        rollback: Query,
        commit: Query,
    )(effect: ZIO[R, E, A])(implicit errorMapper: ErrorMapper[QueryError, E]): ZIO[R, E, A] =
      (
        begin().unit.mapError(errorMapper.mapError) *>
          effect.interruptible
            .foldCauseZIO(
              e =>
                rollback().unit
                  .mapError(errorMapper.mapError)
                  .foldCauseZIO(
                    e2 => ZIO.failCause(Cause.Then(e, e2)),
                    _ => ZIO.failCause(e),
                  ),
              a => commit().unit.mapBoth(errorMapper.mapError, _ => a),
            )
      ).uninterruptible

    def inTransaction[R <: JDBCConnection & Logger & Telemetry, E, A](effect: ZIO[R, E, A])(implicit errorMapper: ErrorMapper[QueryError, E]): ZIO[R, E, A] =
      genericTransaction(
        begin = Transaction.raw.begin,
        rollback = Transaction.raw.rollback,
        commit = Transaction.raw.commit,
      )(effect)

    def inSavepoint[R <: JDBCConnection & Logger & Telemetry, E, A](effect: ZIO[R, E, A])(implicit errorMapper: ErrorMapper[QueryError, E]): ZIO[R, E, A] =
      Savepoint.gen.flatMap { savepoint =>
        genericTransaction(
          begin = Transaction.raw.savepoint(savepoint),
          rollback = Transaction.raw.rollbackSavepoint(savepoint),
          commit = Transaction.raw.releaseSavepoint(savepoint),
        )(effect)
      }

  }

  final class Live[E](con: JDBCConnection)(implicit errorMapper: ErrorMapper[QueryError, E]) extends Transaction[E] {

    override def inTransaction[R <: Logger & Telemetry, A](effect: ZIO[R, E, A]): ZIO[R, E, A] =
      con.use { Helpers.inTransaction(effect) }

    override def inSavepoint[R <: Logger & Telemetry, A](effect: ZIO[R, E, A]): ZIO[R, E, A] =
      con.use { Helpers.inSavepoint(effect) }

  }

  final class UseSavepointForTransaction[E](con: JDBCConnection)(implicit errorMapper: ErrorMapper[QueryError, E]) extends Transaction[E] {

    override def inTransaction[R <: Logger & Telemetry, A](effect: ZIO[R, E, A]): ZIO[R, E, A] =
      con.use { Helpers.inSavepoint(effect) }

    override def inSavepoint[R <: Logger & Telemetry, A](effect: ZIO[R, E, A]): ZIO[R, E, A] =
      con.use { Helpers.inSavepoint(effect) }

  }

  final class RollbackLayer[E](con: JDBCConnection)(implicit errorMapper: ErrorMapper[QueryError, E]) extends Transaction[E] {

    override def inTransaction[R <: Logger & Telemetry, A](effect: ZIO[R, E, A]): ZIO[R, E, A] =
      con.use {
        Helpers.genericTransaction(
          begin = Transaction.raw.begin,
          rollback = Transaction.raw.rollback,
          commit = Transaction.raw.rollback,
        )(effect)
      }

    override def inSavepoint[R <: Logger & Telemetry, A](effect: ZIO[R, E, A]): ZIO[R, E, A] =
      con.use {
        Savepoint.gen.flatMap { savepoint =>
          Helpers.genericTransaction(
            begin = Transaction.raw.savepoint(savepoint),
            rollback = Transaction.raw.rollbackSavepoint(savepoint),
            commit = Transaction.raw.rollbackSavepoint(savepoint),
          )(effect)
        }
      }

  }

  final class Transactionless[E] extends Transaction[E] {
    override def inTransaction[R <: Logger & Telemetry, A](effect: ZIO[R, E, A]): ZIO[R, E, A] = effect
    override def inSavepoint[R <: Logger & Telemetry, A](effect: ZIO[R, E, A]): ZIO[R, E, A] = effect
  }

  def liveLayer[E: Tag](implicit errorMapper: ErrorMapper[QueryError, E]): URLayer[JDBCConnection, Transaction[E]] =
    ZLayer.fromFunction(new Live[E](_))

  def savepointLayer[E: Tag](implicit errorMapper: ErrorMapper[QueryError, E]): URLayer[JDBCConnection, Transaction[E]] =
    ZLayer.fromFunction(new UseSavepointForTransaction[E](_))

  def rollbackLayer[E: Tag](implicit errorMapper: ErrorMapper[QueryError, E]): URLayer[JDBCConnection, Transaction[E]] =
    ZLayer.fromFunction(new RollbackLayer[E](_))

  def transactionlessLayer[E: Tag]: ULayer[Transaction[E]] =
    ZLayer.succeed(new Transactionless[E])

}
