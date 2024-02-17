package harness.sql.query

import cats.syntax.either.*
import harness.sql.*
import harness.sql.error.QueryError
import harness.zio.*
import zio.*

trait Transaction { self =>

  def inTransactionEither[R <: Logger & Telemetry, E, A](effect: ZIO[R, E, A]): ZIO[R, Either[QueryError, E], A]
  final def inTransaction[R <: Logger & Telemetry, E, A](effect: ZIO[R, E, A])(implicit em: ErrorMapper[QueryError, E]): ZIO[R, E, A] =
    self.inTransactionEither(effect).mapError(_.fold(em.mapError, identity))

  def inSavepointEither[R <: Logger & Telemetry, E, A](effect: ZIO[R, E, A]): ZIO[R, Either[QueryError, E], A]
  final def inSavepoint[R <: Logger & Telemetry, E, A](effect: ZIO[R, E, A])(implicit em: ErrorMapper[QueryError, E]): ZIO[R, E, A] =
    self.inSavepointEither(effect).mapError(_.fold(em.mapError, identity))

}
object Transaction {

  def inTransactionEither[R <: Logger & Telemetry, E, A](effect: ZIO[R, E, A]): ZIO[Transaction & R, Either[QueryError, E], A] =
    ZIO.serviceWithZIO[Transaction](_.inTransactionEither(effect))
  def inTransaction[R <: Logger & Telemetry, E, A](effect: ZIO[R, E, A])(implicit em: ErrorMapper[QueryError, E]): ZIO[Transaction & R, E, A] =
    ZIO.serviceWithZIO[Transaction](_.inTransaction(effect))

  // NOTE : These should only be run inside a transaction
  def inSavepointEither[R <: Logger & Telemetry, E, A](effect: ZIO[R, E, A]): ZIO[Transaction & R, Either[QueryError, E], A] =
    ZIO.serviceWithZIO[Transaction](_.inSavepointEither(effect))
  def inSavepoint[R <: Logger & Telemetry, E, A](effect: ZIO[R, E, A])(implicit em: ErrorMapper[QueryError, E]): ZIO[Transaction & R, E, A] =
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

  private object Helpers {

    def genericTransaction[R <: JDBCConnection & Logger & Telemetry, E, A](begin: Query, rollback: Query, commit: Query)(effect: ZIO[R, E, A]): ZIO[R, Either[QueryError, E], A] =
      (
        begin().unit.mapError(_.asLeft) *>
          effect
            .mapError(_.asRight)
            .interruptible
            .foldCauseZIO(
              e =>
                rollback().unit
                  .mapError(_.asLeft)
                  .foldCauseZIO(
                    e2 => ZIO.failCause(Cause.Then(e, e2)),
                    _ => ZIO.failCause(e),
                  ),
              a => commit().unit.mapBoth(_.asLeft, _ => a),
            )
      ).uninterruptible

    def inTransaction[R <: JDBCConnection & Logger & Telemetry, E, A](effect: ZIO[R, E, A]): ZIO[R, Either[QueryError, E], A] =
      genericTransaction(
        begin = Transaction.raw.begin,
        rollback = Transaction.raw.rollback,
        commit = Transaction.raw.commit,
      )(effect)

    def inSavepoint[R <: JDBCConnection & Logger & Telemetry, E, A](effect: ZIO[R, E, A]): ZIO[R, Either[QueryError, E], A] =
      Savepoint.gen.flatMap { savepoint =>
        genericTransaction(
          begin = Transaction.raw.savepoint(savepoint),
          rollback = Transaction.raw.rollbackSavepoint(savepoint),
          commit = Transaction.raw.releaseSavepoint(savepoint),
        )(effect)
      }

  }

  final class Live(con: JDBCConnection) extends Transaction {

    override def inTransactionEither[R <: Logger & Telemetry, E, A](effect: ZIO[R, E, A]): ZIO[R, Either[QueryError, E], A] =
      con.use { Helpers.inTransaction(effect) }

    override def inSavepointEither[R <: Logger & Telemetry, E, A](effect: ZIO[R, E, A]): ZIO[R, Either[QueryError, E], A] =
      con.use { Helpers.inSavepoint(effect) }

  }

  final class UseSavepointForTransaction(con: JDBCConnection) extends Transaction {

    override def inTransactionEither[R <: Logger & Telemetry, E, A](effect: ZIO[R, E, A]): ZIO[R, Either[QueryError, E], A] =
      con.use { Helpers.inSavepoint(effect) }

    override def inSavepointEither[R <: Logger & Telemetry, E, A](effect: ZIO[R, E, A]): ZIO[R, Either[QueryError, E], A] =
      con.use { Helpers.inSavepoint(effect) }

  }

  final class RollbackLayer(con: JDBCConnection) extends Transaction {

    override def inTransactionEither[R <: Logger & Telemetry, E, A](effect: ZIO[R, E, A]): ZIO[R, Either[QueryError, E], A] =
      con.use {
        Helpers.genericTransaction(
          begin = Transaction.raw.begin,
          rollback = Transaction.raw.rollback,
          commit = Transaction.raw.rollback,
        )(effect)
      }

    override def inSavepointEither[R <: Logger & Telemetry, E, A](effect: ZIO[R, E, A]): ZIO[R, Either[QueryError, E], A] =
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

  final class Transactionless extends Transaction {
    override def inTransactionEither[R <: Logger & Telemetry, E, A](effect: ZIO[R, E, A]): ZIO[R, Either[QueryError, E], A] = effect.mapError(_.asRight)
    override def inSavepointEither[R <: Logger & Telemetry, E, A](effect: ZIO[R, E, A]): ZIO[R, Either[QueryError, E], A] = effect.mapError(_.asRight)
  }

  val liveLayer: URLayer[JDBCConnection, Transaction] =
    ZLayer.fromFunction(new Live(_))

  val savepointLayer: URLayer[JDBCConnection, Transaction] =
    ZLayer.fromFunction(new UseSavepointForTransaction(_))

  val rollbackLayer: URLayer[JDBCConnection, Transaction] =
    ZLayer.fromFunction(new RollbackLayer(_))

  val transactionlessLayer: ULayer[Transaction] =
    ZLayer.succeed(new Transactionless)

}
