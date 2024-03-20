package harness.zio.mock

import harness.zio.mock.Types.*

final case class Expectation[Z, I, R, E, A](
    capability: Mock[Z]#Capability[I, R, E, A],
    effect: EffectImpl[I, R, E, A],
)
