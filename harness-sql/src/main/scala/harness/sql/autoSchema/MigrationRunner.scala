package harness.sql.autoSchema

import cats.data.{EitherNel, NonEmptyList}
import cats.syntax.either.*
import cats.syntax.option.*
import harness.core.*
import harness.sql.*
import harness.sql.query.{Fragment, Query, QueryO, Transaction}
import harness.zio.*
import math.Ordering.Implicits.infixOrderingOps
import scala.annotation.tailrec
import zio.*

object MigrationRunner {

  def runMigrations(planned0: InMemoryMigration, plannedN: InMemoryMigration*): SHRIO[JDBCConnection, Unit] =
    doMigrations(planned0 :: plannedN.toList, true)

  def runMigrationsFromPool(planned0: InMemoryMigration, plannedN: InMemoryMigration*): SHRIO[JDBCConnectionPool, Unit] =
    ZIO.scoped {
      MigrationRunner.runMigrations(planned0, plannedN*).provideSomeLayer[HarnessEnv & JDBCConnectionPool & Scope](JDBCConnection.poolLayer)
    }

  private def doMigrations(inMemoryMigrations: List[InMemoryMigration], persist: Boolean): SHRIO[JDBCConnection, Unit] =
    for {
      _ <- Logger.log.info("Validating/running db migration(s)")
      existing <-
        if (persist) createMigrationsTableIfDNE *> getExisting
        else ZIO.succeed(Nil)
      _ <- Logger.log.detailed(s"Planned versions: ${inMemoryMigrations.map(_.version).mkString(", ")}")
      _ <- Logger.log.detailed(s"Already executed versions: ${existing.map(_.version).mkString(", ")}")
      migrationPlans <- ZIO.eitherNelToInternalDefects(createMigrationPlans(None, DbState.initial, existing, inMemoryMigrations, Nil))
      _ <- ZIO.foreachDiscard(migrationPlans)(runMigrationPlan(_, persist))
    } yield ()

  // =====|  |=====

  private object MigrationQueries extends TableQueries[Migration.Id, Migration]

  private val getExisting: SHRIO[JDBCConnection, List[Migration.Identity]] =
    MigrationQueries.selectAll().list.map(_.sortBy(_.version))

  private val getSchemas: QueryO[InformationSchemaSchemata.Identity] = {
    import harness.sql.query.*

    Prepare.selectO("InformationSchemaSchemata - selectAll") {
      Select
        .from[InformationSchemaSchemata]("iss")
        .returning { iss => iss }
    }
  }

  private val createMigrationsTableIfDNE: SHRIO[JDBCConnection, Unit] =
    getSchemas().list.flatMap { schemas =>
      ZIO.unless(schemas.exists(_.schemaName == Migration.tableSchema.tableSchema)) {
        for {
          _ <- Logger.log.detailed("Creating migrations framework")
          _ <- doMigrations(
            InMemoryMigration.auto(Version.parseUnsafe("0.0.0"), Tables(Migration.tableSchema)) :: Nil,
            false,
          )
        } yield ()
      }
    }.unit

  private def failMigrations(msg: String): EitherNel[String, Nothing] =
    s"Expected and Actual migrations differ: $msg".leftNel

  private def createMigrationPlan(
      prevVersion: Option[Version],
      dbStateBefore: DbState,
      dbMigration: Option[Migration.Identity],
      inMemoryMigration: InMemoryMigration,
  ): EitherNel[String, (Version, MigrationPlan)] =
    for {
      _ <- dbMigration match {
        case Some(dbMigration) if dbMigration.version != inMemoryMigration.version => failMigrations(s"Expected migration ${dbMigration.version}, but not got ${inMemoryMigration.version}")
        case _                                                                     => ().asRight
      }
      _ <- prevVersion match {
        case Some(prevVersion) if inMemoryMigration.version <= prevVersion => s"Attempted to create migration ${inMemoryMigration.version} after $prevVersion".leftNel
        case _                                                             => ().asRight
      }
      migrationPlan <- MigrationPlan.make(dbStateBefore, dbMigration, inMemoryMigration)
      _ <- PartialState.validate(PartialState.fromDbState(migrationPlan.resultingDbState), PartialState.fromTables(migrationPlan.tables))
    } yield (inMemoryMigration.version, migrationPlan)

  @tailrec
  private def createMigrationPlans(
      prevVersion: Option[Version],
      dbState: DbState,
      dbMigrations: List[Migration.Identity],
      inMemoryMigrations: List[InMemoryMigration],
      rPlanned: List[MigrationPlan],
  ): EitherNel[String, List[MigrationPlan]] =
    (dbMigrations, inMemoryMigrations) match {
      case (dbMigrationsH :: dbMigrationsT, inMemoryMigrationsH :: inMemoryMigrationsT) =>
        createMigrationPlan(prevVersion, dbState, dbMigrationsH.some, inMemoryMigrationsH) match {
          case Right((version, migrationPlan)) => createMigrationPlans(version.some, migrationPlan.resultingDbState, dbMigrationsT, inMemoryMigrationsT, migrationPlan :: rPlanned)
          case Left(errors)                    => errors.asLeft
        }
      case (Nil, inMemoryMigrationsH :: inMemoryMigrationsT) =>
        createMigrationPlan(prevVersion, dbState, None, inMemoryMigrationsH) match {
          case Right((version, migrationPlan)) => createMigrationPlans(version.some, migrationPlan.resultingDbState, Nil, inMemoryMigrationsT, migrationPlan :: rPlanned)
          case Left(errors)                    => errors.asLeft
        }
      case (Nil, Nil)                => rPlanned.reverse.asRight
      case (dbMigrationsH :: _, Nil) => failMigrations(s"Expected migration ${dbMigrationsH.version}, but not found")
    }

  private def executeEffect(effect: MigrationEffect): SHRIO[JDBCConnection, Unit] =
    effect match {
      case MigrationEffect.Code(name, code) => Logger.log.detailed(s"Running migration step code '$name'") *> code
      case MigrationEffect.Sql(sql)         => new Query("Migration Step", Fragment.fromString(sql)).apply().unit
    }

  private def runMigrationPlan(migrationPlan: MigrationPlan, persist: Boolean): SHRIO[JDBCConnection, Unit] =
    if (migrationPlan.run)
      Transaction
        .inTransaction {
          for {
            _ <- Logger.log.info(s"Running db migration ${migrationPlan.version}")
            _ <- ZIO.foreachDiscard(migrationPlan.steps.toList) { step => executeEffect(step.effect) }
            _ <- MigrationQueries.insert(migrationPlan.migration).single.when(persist)
          } yield ()
        }
        .provideSomeLayer[HarnessEnv & JDBCConnection](Transaction.liveLayer)
    else
      Logger.log.info(s"Skipping db migration ${migrationPlan.version} (already ran)")

}
