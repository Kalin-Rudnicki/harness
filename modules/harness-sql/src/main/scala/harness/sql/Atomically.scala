package harness.sql

import harness.sql.error.*
import harness.sql.query.*
import harness.zio.*
import java.util.UUID
import zio.*

trait Atomically[E] {
  def atomicScope: ZIO[Scope, E, Unit]
  def atomically[R, A](effect: ZIO[R, E, A]): ZIO[R, E, A]
  final def apply[R, A](effect: ZIO[R, E, A]): ZIO[R, E, A] = atomically(effect)
}
object Atomically {

  // =====|  |=====

  def atomicScope[E: Tag]: ZIO[Atomically[E] & Scope, E, Unit] =
    ZIO.serviceWithZIO[Atomically[E]](_.atomicScope)

  def atomically[R, E: Tag, A](effect: ZIO[R, E, A]): ZIO[Atomically[E] & R, E, A] =
    ZIO.serviceWithZIO[Atomically[E]](_.atomically(effect))

  // =====|  |=====

  sealed trait WithConnection[E] extends Atomically[E] {

    val con: Database
    val mapError: ErrorMapper[QueryError, E]

    private given ErrorMapper[QueryError, E] = mapError

    def commit(connectionType: Database.Current.ConnectionType): Query

    private def finalizeEffect(con: Database.Current.ConstConnection, exit: Exit[?, ?]): URIO[Database, Unit] = exit match
      case Exit.Success(_) => Logger.log.debug(s"committing on connection ${con.connection.id}") *> commit(con.connectionType)().unit.orDie
      case Exit.Failure(_) => Logger.log.debug(s"rolling back on connection ${con.connection.id}") *> queries.rollback(con.connectionType)().unit.orDie

    override def atomicScope: ZIO[Scope, E, Unit] =
      con.use {
        con.getAtomicChild
          .mapError { e => mapError.mapError(QueryError("atomically", "<???>", QueryError.Cause.ErrorGettingConnection(e))) }
          .withFinalizerExit(finalizeEffect)
          .tap { con => queries.begin(con.connectionType)().unit.mapErrorTo[E] }
          .unit
      }

    override final def atomically[R, A](effect: ZIO[R, E, A]): ZIO[R, E, A] =
      ZIO.scoped { atomicScope *> effect }

  }

  /**
    * Uses a live connection.
    * If the given effect passes, it will be committed.
    * If the given effect fails, it will be rolled back.
    */
  final case class Live[E](
      con: Database,
      mapError: ErrorMapper[QueryError, E],
  ) extends Atomically.WithConnection[E] {
    override def commit(connectionType: Database.Current.ConnectionType): Query = queries.commit(connectionType)
  }
  object Live {

    def layer[E: Tag](implicit mapError: ErrorMapper[QueryError, E]): URLayer[Database, Atomically[E]] =
      ZLayer.service[Database].project(Live(_, mapError))

    def atomically[R, E: Tag, A](effect: ZIO[R, E, A])(implicit errorMapper: ErrorMapper[QueryError, E]): ZIO[R & Database, E, A] =
      ZIO.serviceWithZIO[Atomically[E]](_.atomically(effect)).provideSomeLayer[R & Database](Live.layer[E])

    def atomicScope[E: Tag](implicit errorMapper: ErrorMapper[QueryError, E]): ZIO[Database & Scope, E, Unit] =
      ZIO.serviceWithZIO[Atomically[E]](_.atomicScope).provideSomeLayer[Database & Scope](Live.layer[E])

  }

  /**
    * Uses a live connection.
    * Whether or not the effect passes, rollback will occur.
    */
  final case class LiveRollback[E](
      con: Database,
      mapError: ErrorMapper[QueryError, E],
  ) extends Atomically.WithConnection[E] {
    override def commit(connectionType: Database.Current.ConnectionType): Query = queries.rollbackRoot(connectionType)
  }
  object LiveRollback {

    def layer[E: Tag](implicit mapError: ErrorMapper[QueryError, E]): URLayer[Database, Atomically[E]] =
      ZLayer.service[Database].project(LiveRollback(_, mapError))

  }

  /**
    * Will ignore the fact you are trying to do something atomically,
    * and execute the effect as-is.
    */
  final class IgnoreAtomicity[E] extends Atomically[E] {
    override def atomicScope: ZIO[Scope, E, Unit] = ZIO.unit
    override def atomically[R, A](effect: ZIO[R, E, A]): ZIO[R, E, A] = effect
  }
  object IgnoreAtomicity {

    def layer[E: Tag]: ULayer[Atomically[E]] =
      ZLayer.succeed(new IgnoreAtomicity[E])

  }

  private object queries {

    def begin(connectionType: Database.Current.ConnectionType): Query = connectionType match
      case Database.Current.ConnectionType.Transaction            => raw.begin
      case Database.Current.ConnectionType.Savepoint(savepointId) => raw.savepoint(savepointId)

    def commit(connectionType: Database.Current.ConnectionType): Query = connectionType match
      case Database.Current.ConnectionType.Transaction            => raw.commit
      case Database.Current.ConnectionType.Savepoint(savepointId) => raw.releaseSavepoint(savepointId)

    def rollback(connectionType: Database.Current.ConnectionType): Query = connectionType match
      case Database.Current.ConnectionType.Transaction            => raw.rollback
      case Database.Current.ConnectionType.Savepoint(savepointId) => raw.rollbackSavepoint(savepointId)

    def rollbackRoot(connectionType: Database.Current.ConnectionType): Query = connectionType match
      case Database.Current.ConnectionType.Transaction            => raw.rollback
      case Database.Current.ConnectionType.Savepoint(savepointId) => raw.releaseSavepoint(savepointId)

    private object raw {

      val begin: Query = Query("BEGIN", fr"BEGIN")

      val commit: Query = Query("COMMIT", fr"COMMIT")

      val rollback: Query = Query("ROLLBACK", fr"ROLLBACK")

      def savepoint(savepointId: UUID): Query = Query("SAVEPOINT", fr"SAVEPOINT ${savepointId.toString}")

      def releaseSavepoint(savepointId: UUID): Query = Query("RELEASE SAVEPOINT", fr"RELEASE SAVEPOINT ${savepointId.toString}")

      def rollbackSavepoint(savepointId: UUID): Query = Query("ROLLBACK TO SAVEPOINT", fr"ROLLBACK TO SAVEPOINT ${savepointId.toString}")

    }

  }

}
