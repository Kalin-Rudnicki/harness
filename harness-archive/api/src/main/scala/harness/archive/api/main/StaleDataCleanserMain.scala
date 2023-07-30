package harness.archive.api.main

import harness.archive.api.service.StaleDataCleanser
import harness.archive.api.service.storage.*
import harness.sql.*
import harness.sql.query.Transaction
import harness.zio.*
import zio.*

object StaleDataCleanserMain {

  private type Env =
    StaleDataCleanser

  private val envLayer: SHRLayer[Scope, Env] =
    Shared.poolLayer >>>
      StaleDataCleanser.live(1.minute, 1.minute, 1.minute, 5.minutes, 15.minutes)

  val executable: Executable =
    Executable
      .withLayer[Env] { envLayer }
      .withEffect { StaleDataCleanser.startFiber }

}
