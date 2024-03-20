package harness.zio.test.mock

import harness.zio.test.mock.Types.*

final case class SeededExpectation[Z, I, R, E, A](
    capability: Mock[Z]#Capability[I, R, E, A],
    effect: EffectImpl[I, R, E, A],
)
