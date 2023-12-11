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

    def validateSteps(version: Version, ran: List[MigrationStep.Encoded], generated: List[MigrationStep.Encoded]): EitherNel[String, Unit] =
      if (ran == generated) ().asRight
      else if (ran.size == generated.size && ran.toSet == generated.toSet) {
        val filtered = ran.zip(generated).zipWithIndex.collect { case ((r, g), i) if r != g => s"\n  [$i]: ran=$r / generated=$g" }
        s"Migration $version is not as expected. All elements are the same, but out of order:${filtered.mkString}".leftNel
      } else {
        val ranSet = ran.toSet
        val generatedSet = generated.toSet
        val ranButNotGenerated = ran.filterNot(generatedSet.contains)
        val generatedButNotRan = generated.filterNot(ranSet.contains)

        s"""Migration $version has mismatched elements...
           |  Ran but not generated:${ranButNotGenerated.map { step => s"\n    - $step" }.mkString}
           |  Generated but not ran:${generatedButNotRan.map { step => s"\n    - $step" }.mkString}""".stripMargin.leftNel
      }

    def validateMigration(ranMigration: Migration.Identity, generatedMigration: Migration.Identity): EitherNel[String, Unit] =
      if (ranMigration.version != generatedMigration.version) s"Ran and generated migrations have different versions: ${ranMigration.version} / ${generatedMigration.version}".leftNel
      else validateSteps(ranMigration.version, ranMigration.steps.toList, generatedMigration.steps.toList)

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

    if (inMemoryMigration.version == Version.make(0)) "Migration version must be greater than 0".leftNel
    else convertMigrationSteps(initialDbState, inMemoryMigration.steps.toList, Nil)
  }

}
