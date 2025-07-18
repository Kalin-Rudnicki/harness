package harness.sql.autoSchema

import cats.data.{EitherNel, NonEmptyList}
import cats.syntax.either.*
import cats.syntax.option.*
import harness.cli.*
import harness.core.*
import harness.sql.*
import harness.sql.error.*
import harness.sql.query.{Fragment, Query, QueryO}
import harness.zio.*
import scala.annotation.tailrec
import scala.math.Ordering.Implicits.infixOrderingOps
import zio.*
import zio.json.JsonDecoder

object MigrationRunner {

  def runMigrations(migrations: PlannedMigrations): ZIO[Database, MigrationError, Unit] =
    doMigrations(migrations.migrations.toList, true)

  def rollbackTo(version: Version): ZIO[Database, MigrationError, Unit] =
    for {
      _ <- Logger.log.important(s"RESETTING DATABASE to $version")
      existing <- getExisting
      rollbacks <- reverseTo(version, existing.reverse, Nil)
      _ <- Logger.log.warning("No migrations to rollback").when(rollbacks.isEmpty)
      _ <- ZIO.foreachDiscard(rollbacks)(undoMigration)
    } yield ()

  def rollbackExecutable[JsonConfig: JsonDecoder](f: JsonConfig => DbConfig): Executable =
    Executable
      .withConfig[JsonConfig]
      .withCommandLine { Params.value[Version]("version") }
      .withEnv { (cfg, _) => ZLayer.succeed(f(cfg)) >>> Database.poolLayer }
      .implementC { MigrationRunner.rollbackTo(_) }

  // =====|  |=====

  private def doMigrations(inMemoryMigrations: List[InMemoryMigration], persist: Boolean): ZIO[Database, MigrationError, Unit] =
    for {
      _ <- Logger.log.info("Validating/running db migration(s)")
      existing <-
        if (persist) createMigrationsTableIfDNE *> getExisting
        else ZIO.succeed(Nil)
      _ <- Logger.log.detailed(s"Planned versions: ${inMemoryMigrations.map(_.version).mkString(", ")}")
      _ <- Logger.log.detailed(s"Already executed versions: ${existing.map(_.version).mkString(", ")}")
      migrationPlans <- createMigrationPlans(None, DbState.initial, existing, inMemoryMigrations, Nil) match {
        case Right(value) => ZIO.succeed(value)
        case Left(errors) => ZIO.fail(MigrationError.Invalid(errors.toList.mkString("\n")))
      }
      _ <- ZIO.foreachDiscard(migrationPlans)(runMigrationPlan(_, persist))
    } yield ()

  // =====|  |=====

  private object MigrationQueries extends TableQueries[Migration.Id, Migration]

  private val getExisting: ZIO[Database, MigrationError, List[Migration.Identity]] =
    MigrationQueries.selectAll().list.map(_.sortBy(_.version)).mapError(MigrationError.QueryError(_))

  private val getSchemas: QueryO[InformationSchemaSchemata.Identity] = {
    import harness.sql.query.*

    Prepare.selectO("InformationSchemaSchemata - selectAll") {
      Select
        .from[InformationSchemaSchemata]("iss")
        .returning { iss => iss }
    }
  }

  private val createMigrationsTableIfDNE: ZIO[Database, MigrationError, Unit] =
    getSchemas().list
      .mapError(MigrationError.QueryError(_))
      .flatMap { schemas =>
        ZIO.unlessDiscard(schemas.exists(_.schemaName == Migration.tableSchema.tableSchema)) {
          Logger.log.detailed("Creating migrations framework") *>
            doMigrations(InMemoryMigration.auto(Version.make(0, 0, 0, 1), Tables(Migration.tableSchema)) :: Nil, false)
        }
      }

  private def failMigrations(msg: String): EitherNel[String, Nothing] =
    s"Expected and Actual migrations differ: $msg".leftNel

  private def createMigrationPlan(
      prevVersion: Option[Version],
      dbStateBefore: DbState,
      dbMigration: Option[Migration.Identity],
      inMemoryMigration: InMemoryMigration,
  ): EitherNel[String, (Version, MigrationPlan)] =
    for {
      _ <- prevVersion match {
        case Some(prevVersion) if inMemoryMigration.version <= prevVersion => failMigrations(s"Version ${inMemoryMigration.version} is not greater than $prevVersion")
        case _                                                             => ().asRight
      }
      _ <- dbMigration match {
        case Some(dbMigration) if dbMigration.version != inMemoryMigration.version => failMigrations(s"Expected migration ${dbMigration.version}, but not got ${inMemoryMigration.version}")
        case _                                                                     => ().asRight
      }
      _ <- prevVersion match {
        case Some(prevVersion) if inMemoryMigration.version < prevVersion => s"Attempted to create migration ${inMemoryMigration.version} after $prevVersion".leftNel
        case _                                                            => ().asRight
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

  private def executeEffect(effect: MigrationEffect): ZIO[Database, MigrationError, Unit] =
    effect match {
      case MigrationEffect.Code(name, code) => Logger.log.detailed(s"Running migration step code '$name'") *> code.mapError(MigrationError.UnableToExecuteCodeStep(_))
      case MigrationEffect.Sql(sql)         => Query("Migration Step", Fragment.sql(sql)).apply().unit.mapError(MigrationError.QueryError(_))
    }

  private def runMigrationPlan(migrationPlan: MigrationPlan, persist: Boolean): ZIO[Database, MigrationError, Unit] =
    if (migrationPlan.run)
      Atomically.Live.atomically {
        for {
          _ <- Logger.log.info(s"Running db migration ${migrationPlan.version}")
          _ <- ZIO.foreachDiscard(migrationPlan.steps.toList) { step => executeEffect(step.effect) }
          _ <- MigrationQueries.insert(migrationPlan.migration).single.when(persist).mapError(MigrationError.QueryError(_))
        } yield ()
      }
    else
      Logger.log.info(s"Skipping db migration ${migrationPlan.version} (already ran)")

  @tailrec
  private def reverseTo(version: Version, rQueue: List[Migration.Identity], rStack: List[Migration.Identity]): IO[MigrationError, List[Migration.Identity]] =
    rQueue match {
      case head :: tail =>
        if (head.version == version) ZIO.succeed((head :: rStack).reverse)
        else if (head.version > version) reverseTo(version, tail, head :: rStack)
        else ZIO.fail(MigrationError.Invalid(s"Missing version '$version' to rollback to"))
      case Nil =>
        if (version == Version.zero) ZIO.succeed(rStack.reverse)
        else ZIO.fail(MigrationError.Invalid(s"No version '$version' to rollback to"))
    }

  private def undoMigration(migration: Migration.Identity): ZIO[Database, MigrationError, Unit] =
    Atomically.Live.atomically {
      for {
        _ <- Logger.log.info(s"Undoing migration ${migration.version}")
        reversedOptSteps <- ZIO.foreach(migration.steps.toList.reverse) {
          case MigrationStep.Encoded.Code(name, reversible)                => Logger.log.warning(s"Ignoring code step '$name', reversible=$reversible").as(None)
          case MigrationStep.CreateSchema(ref)                             => ZIO.some(MigrationStep.DropSchema(ref))
          case MigrationStep.RenameSchema(refBefore, refAfter)             => ZIO.some(MigrationStep.RenameSchema(refAfter, refBefore))
          case MigrationStep.DropSchema(ref)                               => ZIO.some(MigrationStep.CreateSchema(ref))
          case MigrationStep.CreateTable(ref)                              => ZIO.some(MigrationStep.DropTable(ref))
          case MigrationStep.RenameTable(schemaRef, nameBefore, nameAfter) => ZIO.some(MigrationStep.RenameTable(schemaRef, nameAfter, nameBefore))
          case MigrationStep.DropTable(ref)                                => ZIO.some(MigrationStep.CreateTable(ref))
          case MigrationStep.CreateCol(ref, _, keyType, _)                 => ZIO.some(MigrationStep.DropCol(ref, keyType))
          case MigrationStep.RenameCol(tableRef, nameBefore, nameAfter)    => ZIO.some(MigrationStep.RenameCol(tableRef, nameAfter, nameBefore))
          case MigrationStep.DropCol(ref, _)                               => ZIO.fail(MigrationError.Invalid(s"Unable to reverse DropCol '$ref'"))
          case MigrationStep.SetColNotNullable(ref)                        => ZIO.some(MigrationStep.SetColNullable(ref))
          case MigrationStep.SetColNullable(ref)                           => ZIO.some(MigrationStep.SetColNotNullable(ref))
          case MigrationStep.CreateIndex(tableRef, name, _, _)             => ZIO.some(MigrationStep.DropIndex(tableRef.schemaRef, name))
          case MigrationStep.RenameIndex(nameBefore, nameAfter)            => ZIO.some(MigrationStep.RenameIndex(nameAfter, nameBefore))
          case MigrationStep.DropIndex(_, name)                            => ZIO.fail(MigrationError.Invalid(s"Unable to reverse DropIndex '$name'"))
        }
        reversedSteps = reversedOptSteps.flatten
        _ <- ZIO.foreachDiscard(reversedSteps) { step => executeEffect(MigrationEffect.Sql(step.sql)) }
        _ <- MigrationQueries.deleteById(migration.id).single.mapError(MigrationError.QueryError(_))
      } yield ()
    }

  private implicit val mapQueryError: ErrorMapper[QueryError, MigrationError] = MigrationError.QueryError(_)

}
