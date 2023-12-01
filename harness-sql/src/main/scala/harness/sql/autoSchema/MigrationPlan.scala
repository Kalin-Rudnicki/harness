package harness.sql.autoSchema

import cats.data.{EitherNel, NonEmptyList}
import cats.syntax.either.*
import cats.syntax.traverse.*
import harness.core.Version
import scala.annotation.tailrec

final case class MigrationPlan private (
    version: Version,
    steps: NonEmptyList[MigrationPlan.Step],
    initialDbState: DbState,
    resultingDbState: DbState,
    tables: Tables,
    migration: Migration.Identity,
    run: Boolean,
)
object MigrationPlan {

  final case class Step(
      inMemory: MigrationStep.InMemory,
      encoded: MigrationStep.Encoded,
      effect: MigrationEffect,
  )

  def make(initialDbState: DbState, migration: Option[Migration.Identity], inMemoryMigration: InMemoryMigration): EitherNel[String, MigrationPlan] = {
    @tailrec
    def convertInMemorySteps(state: DbState, queue: List[MigrationStep.InMemory], rStack: List[MigrationPlan.Step]): EitherNel[String, (DbState, List[MigrationPlan.Step])] =
      queue match {
        case head :: tail =>
          DbState.next(state, head) match {
            case Right((effect, encoded, newState)) => convertInMemorySteps(newState, tail, MigrationPlan.Step(head, encoded, effect) :: rStack)
            case Left(error)                        => error.leftNel
          }
        case Nil => (state, rStack.reverse).asRight
      }

    def step(state: DbState, step: InMemoryMigration.Step): EitherNel[String, (DbState, List[MigrationPlan.Step])] =
      step match {
        case InMemoryMigration.Step.Auto(tables, allowDrops) =>
          val exp = PartialState.fromTables(tables)
          val partialState = PartialState.fromDbState(state)
          PartialState.diff(partialState, exp, allowDrops).flatMap(convertInMemorySteps(state, _, Nil))
        case InMemoryMigration.Step.Manual(step) =>
          convertInMemorySteps(state, step :: Nil, Nil)
      }

    @tailrec
    def validateSteps(stepNo: Int, ran: List[MigrationStep.Encoded], generated: List[MigrationStep.Encoded]): EitherNel[String, Unit] =
      (ran, generated) match {
        case (ranH :: ranT, generatedH :: generatedT) =>
          if (ranH == generatedH) validateSteps(stepNo + 1, ranT, generatedT)
          else s"Migration ${inMemoryMigration.version} step $stepNo : Expected $ranH, got: $generatedH".leftNel
        case (ranH :: _, Nil) =>
          s"Migration ${inMemoryMigration.version} step $stepNo : Expected $ranH, got: No Step".leftNel
        case (Nil, generatedH :: _) =>
          s"Migration ${inMemoryMigration.version} step $stepNo : Expected No Step, got: $generatedH".leftNel
        case (Nil, Nil) => ().asRight
      }

    def validateMigration(ranMigration: Migration.Identity, generatedMigration: Migration.Identity): EitherNel[String, Unit] =
      if (ranMigration.version != generatedMigration.version) s"Ran and generated migrations have different versions: ${ranMigration.version} / ${generatedMigration.version}".leftNel
      else validateSteps(1, ranMigration.steps.toList, generatedMigration.steps.toList)

    @tailrec
    def convertMigrationSteps(state: DbState, queue: List[InMemoryMigration.Step], rStack: List[List[MigrationPlan.Step]]): EitherNel[String, MigrationPlan] =
      queue match {
        case head :: tail =>
          step(state, head) match {
            case Right((newState, newSteps)) => convertMigrationSteps(newState, tail, newSteps :: rStack)
            case Left(errors)                => errors.asLeft
          }
        case Nil =>
          NonEmptyList.fromList(rStack.reverse.flatten) match {
            case Some(steps) =>
              val generatedMigration =
                new Migration.Identity(
                  id = Migration.Id.gen,
                  version = inMemoryMigration.version,
                  steps = steps.map(_.encoded),
                )

              migration.traverse(validateMigration(_, generatedMigration)).map { abc =>
                MigrationPlan(
                  inMemoryMigration.version,
                  steps,
                  initialDbState,
                  state,
                  inMemoryMigration.tables,
                  generatedMigration,
                  abc.isEmpty,
                )
              }
            case None =>
              s"Migration ${inMemoryMigration.version} has no steps".leftNel
          }
      }

    convertMigrationSteps(initialDbState, inMemoryMigration.steps.toList, Nil)
  }

}
