package harness.zio.mock

import zio.*

object Types {

  type EffectImpl[I, R, E, A] = I => ZIO[R, E, A]
  type ErasedEffectImpl = ? => ZIO[?, ?, ?]

  type ErasedCapabilityZ[Z] = Mock[Z]#Capability[?, ?, ?, ?]
  type ErasedCapability = Mock[?]#Capability[?, ?, ?, ?]

  type ErasedExpectation = Expectation[?, ?, ?, ?, ?]

}
