package harness.sql

import harness.sql.autoSchema.*
import harness.sql.error.*
import harness.zio.*
import java.util.UUID
import zio.*
import zio.stream.*

/**
  * This represents access to an SQL database.
  * When accessing your database, it could be in one of 3 states:
  *   - Pool : you are not in a transaction, and are able to execute as many parallel fibers as your pool will allow.
  *   - Transaction : you are in a db transaction, and are only able to execute 1 parallel fiber at a time.
  *   - Savepoint : you are in a db transaction + savepoint, and are only able to execute 1 parallel fiber at a time.
  *
  * You can get a Transaction/Savepoint by using [[Atomically.atomically]]/[[Atomically.atomicScope]].
  * These can also be accessed via: [[Atomically.Live$.atomically]]/[[Atomically.Live$.atomicScope]].
  * The reason Transaction/Savepoint only support a single parallel fiber is because anything happening atomically in the db needs to be on a single connection,
  * and transactions/savepoints are scoped to the entire connection.
  * Otherwise, you could end up with a scenario like:
  *   - in transaction
  *     - fiber-a : in savepoint
  *     - fiber-b : in savepoint
  *     - fiber-a : insert R1
  *     - fiber-b : insert R2
  *     - fiber-a : insert R3
  *     - fiber-a : rollback (insertion of R2 is also rolled back)
  */
final class Database private (ref: FiberRef[Database.Current]) {

  def use[R, E, A](zio: ZIO[R & Database, E, A]): ZIO[R, E, A] =
    zio.provideSomeEnvironment[R](_.add(this))

  def use[R, E, A](zio: ZStream[R & Database, E, A]): ZStream[R, E, A] =
    zio.provideSomeEnvironment[R](_.add(this))

  private[sql] def getConnection: ZIO[Scope, ConnectionError, JDBCConnection] =
    ref.getWith(_.getConnection)

  private[sql] def getAtomicChild: ZIO[Scope, ConnectionError, Database.Current.ConstConnection] =
    ref.getWith(_.getAtomicChild).tap { ref.locallyScoped(_) }

}
object Database {

  // =====|  |=====

  def pool(
      connectionFactory: ConnectionFactory,
      range: Range,
      duration: Duration,
      logging: DbConfig.Logging,
  ): URIO[Scope, Database] =
    for {
      pool <- LoggedZPool.make[Any, ConnectionError, JDBCConnection](
        "Connection Pool",
        _.id.toString,
        Logger.LogLevel.Trace,
        connectionFactory.getJDBCConnection,
        range,
        duration,
      )
      ref <- FiberRef.make[Current](Current.Root(pool, logging))
    } yield Database(ref)

  def poolLayer(
      connectionFactory: ConnectionFactory,
      range: Range,
      duration: Duration,
      logging: DbConfig.Logging,
  ): ULayer[Database] =
    ZLayer.scoped { pool(connectionFactory, range, duration, logging) }

  /**
    * Creates a pool layer from a [[DbConfig]].
    */
  def poolLayer: URLayer[DbConfig, Database] =
    ZLayer.scoped {
      ZIO.serviceWithZIO[DbConfig] { dbConfig =>
        pool(
          ConnectionFactory(dbConfig),
          dbConfig.pool.minConnections to dbConfig.pool.maxConnections,
          dbConfig.pool.duration,
          dbConfig.logging,
        )
      }
    }

  /**
    * Creates a pool layer from a [[DbConfig]].
    * Then, runs the given migrations against that database.
    */
  def poolLayerWithMigrations(
      migration0: InMemoryMigration,
      migrationN: InMemoryMigration*,
  ): ZLayer[DbConfig, MigrationError, Database] =
    poolLayer
      .tap { databaseEnv => MigrationRunner.runMigrations(PlannedMigrations(migration0, migrationN*)).provideEnvironment(databaseEnv) }

  /**
    * Creates a pool layer from a [[DbConfig]].
    * Then, runs the given migrations against that database.
    */
  def poolLayerWithMigrations(
      migrations: PlannedMigrations,
  ): ZLayer[DbConfig, MigrationError, Database] =
    poolLayer
      .tap { databaseEnv => MigrationRunner.runMigrations(migrations).provideEnvironment(databaseEnv) }

  private[sql] def getConnection: ZIO[Database & Scope, ConnectionError, JDBCConnection] =
    ZIO.serviceWithZIO[Database](_.getConnection)

  // =====|  |=====

  private[sql] sealed trait Current {
    def getConnection: ZIO[Scope, ConnectionError, JDBCConnection]
    def getAtomicChild: ZIO[Scope, ConnectionError, Current.ConstConnection]
  }
  private[sql] object Current {

    enum ConnectionType {
      case Transaction
      case Savepoint(savepointId: UUID)
    }

    sealed trait ConstConnection extends Current {

      val connection: JDBCConnection
      val mutex: Semaphore
      val connectionType: ConnectionType

      override final def getConnection: ZIO[Scope, ConnectionError, JDBCConnection] =
        mutex.withPermitScoped.as(connection)

      override final def getAtomicChild: ZIO[Scope, ConnectionError, Current.ConstConnection] =
        for {
          _ <- mutex.withPermitScoped
          savepointId <- Random.nextUUID
          mutex2 <- Semaphore.make(1L)
        } yield Current.Savepoint(connection, mutex2, savepointId)

    }

    final case class Root(pool: ZPool[ConnectionError, JDBCConnection], logging: DbConfig.Logging) extends Current {

      override def getConnection: ZIO[Scope, ConnectionError, JDBCConnection] =
        pool.get
          .tap { con => Logger.addContext("db-connection-id" -> con.id).setScoped.whenDiscard(logging.addConnectionContext) }

      override def getAtomicChild: ZIO[Scope, ConnectionError, Current.ConstConnection] =
        for {
          connection <- pool.get
          mutex <- Semaphore.make(1L)
          transactionId <- Random.nextUUID
          _ <- Logger.addContext("db-transaction-id" -> transactionId).setScoped.whenDiscard(logging.addTransactionContext)
        } yield Current.Transaction(connection, mutex, transactionId)

    }

    final case class Transaction(connection: JDBCConnection, mutex: Semaphore, transactionId: UUID) extends Current.ConstConnection {
      override val connectionType: ConnectionType = ConnectionType.Transaction
    }

    final case class Savepoint(connection: JDBCConnection, mutex: Semaphore, savepointId: UUID) extends Current.ConstConnection {
      override val connectionType: ConnectionType = ConnectionType.Savepoint(savepointId)
    }

    // TODO (KR) :

  }

}
